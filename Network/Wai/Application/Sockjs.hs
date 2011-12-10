{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns, Rank2Types, ExistentialQuantification #-}
module Network.Wai.Application.Sockjs
  ( WSLiteRoute
  , waiApps
  , wsApps
  , waiRoute
  , newSockjsState
  ) where

-- imports {{{

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.List
import           Data.Maybe
import           Data.IORef
import           Data.Int (Int64)

import           Prelude hiding (catch)
import           Control.Exception (catch, throw, mask, onException, SomeException)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad (forever, msum, when)
import           Control.Applicative ( (<$>) )
import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Concurrent.Timer

import           Data.ByteString (ByteString)
import           Data.ByteString.Utils (toLazy, toStrict)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import           Data.Enumerator ( Enumerator, Enumeratee, run_, ($$), ($=), (=$), (>==>), Stream(..), returnI )
import qualified Data.Enumerator.Utils as E
import qualified Data.Enumerator.List as EL
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Encode as JSON
import           Data.Digest.Pure.MD5 (md5)
import qualified Data.Binary as Binary

import           Network.HTTP.Types
import           Network.Wai
import           Web.Cookie
import qualified Network.WebSockets as WS
import           Network.WebSockets.Lite
import           Network.WebSockets.Lite.Emulate
import           Network.Sockjs

-- }}}

responseLimit :: Int64
responseLimit = 128*1024
heartBeatInterval :: Int
heartBeatInterval = 5

type WSLiteRoute = [([Text], (WSLite (), Maybe [Text]))]

waiApps :: MVar SessionMap -> WSLiteRoute -> Application
waiApps state apps req = case (requestMethod req, matchResult) of
    ("OPTIONS", Just _) -> return $ optionsResponse req

    (m, Just ((app, disallows), path)) -> case (m, path) of

        ("GET", _) | path==[] || path==[""] ->
            return $ ok hsPlain $ B.fromByteString "Welcome to SockJS!\n"
        ("GET", [ isIframe -> True ]) ->
            return $ iframeResponse req
        ("POST", ["chunking_test"]) ->
            return $ chunkingTestResponse req

        (_, [server, sid, path'])
          | maybe False (elem path') disallows
            || T.null server || T.null sid
            || T.any (=='.') server || T.any (=='.') sid ->
                return $ notFound []

        ("POST", [_, sid, "xhr"]) ->
            downstream state sid app 1 returnI hsJavascript newline req

        ("POST", [_, sid, "xhr_streaming"]) ->
            let prelude = E.enumSingle $ S.replicate 2048 'h' `mappend` "\n"
            in  downstream state sid app responseLimit prelude hsJavascript newline req

        ("GET", [_, sid, "jsonp"]) ->
            let wrap callback b = mconcat
                    [ B.fromByteString callback
                    , B.fromByteString "("
                    , JSON.fromValue . JSON.toJSON . B.toByteString $ b
                    , B.fromByteString ");\r\n"
                    ]
            in withCallback $ \callback -> 
                   downstream state sid app 1 returnI hsJavascript (wrap callback) req

        ("GET", [_, sid, "htmlfile"]) ->
            withCallback $ \callback ->
                downstream state sid app responseLimit (htmlfile callback) hsHtml wrap req
          where 
            wrap b = mconcat [ B.fromByteString "<script>\np("
                             , JSON.fromValue . JSON.toJSON . B.toByteString $ b
                             , B.fromByteString ");\n</script>\r\n"
                             ]
            htmlfile callback = E.enumSingle $
                "<!doctype html>\n\
                \<html><head>\n\
                \  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
                \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
                \</head><body><h2>Don't panic!</h2>\n\
                \  <script>\n\
                \    document.domain = document.domain;\n\
                \    var c = parent." `mappend` callback `mappend` ";\n\
                \    c.start();\n\
                \    function p(d) {c.message(d);};\n\
                \    window.onload = function() {c.stop();};\n\
                \  </script>" `mappend` S.replicate 658 ' ' -- ensure 1024 length

        ("GET", [_, sid, "eventsource"]) ->
            let wrap b = mconcat
                  [ B.fromByteString "data: "
                  , b
                  , B.fromByteString "\r\n\r\n"
                  ]
                prelude = E.enumSingle "\r\n"
            in  downstream state sid app responseLimit prelude hsEventStream wrap req

        ("POST", [_, sid, "xhr_send"]) ->
            upstream state sid id (noContent req) req

        ("POST", [_, sid, "jsonp_send"]) ->
            upstream state sid parse (ok (hsCookie req) $ B.fromByteString "ok") req
          where
            ct = fromMaybe "application/x-www-form-urlencoded" $ lookup "Content-Type" $ requestHeaders req
            parse body = case ct of
                "application/x-www-form-urlencoded" ->
                    case lookup "d" $ parseQuery (toStrict body) of
                        Just (Just s) -> toLazy s
                        _ -> mempty
                _ -> body

        _ -> return $ notFound []
    _ -> return $ notFound []
  where
    withCallback f = case lookup "c" (queryString req) of
        Just (Just callback) | not (S.null callback) -> f callback
        _ -> return $ serverError "\"callback\" parameter required"

    matchResult = msum (map match apps)
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

    isIframe p = T.isPrefixOf "iframe" p && T.isSuffixOf ".html" p

-- response utils {{{

newline :: Builder -> Builder
newline = (`mappend` B.fromByteString "\n")

hsJavascript, hsPlain, hsHtml, hsEventStream, hsCache, hsNoCache :: [Header]
hsJavascript  = [("Content-Type", "application/javascript; charset=UTF-8")]
hsPlain       = [("Content-Type", "text/plain; charset=UTF-8")]
hsHtml        = [("Content-Type", "text/html; charset=UTF-8")]
hsEventStream = [("Content-Type", "text/event-stream; charset=UTF-8")]
hsCache       = [("Cache-Control", "public; max-age=31536000;")
                ,("Expires", "31536000")]
hsNoCache     = [("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")]
hsAC :: Request -> [Header]
hsAC req = [("access-control-max-age", "31536000")
           ,("access-control-allow-origin", origin)
           ,("access-control-allow-credentials", "true")]
  where origin = fromMaybe "*" $ lookup "Origin" $ requestHeaders req

hsCookie :: Request -> [Header]
hsCookie req = [("Set-Cookie", mconcat ["JSESSIONID=", jsid, "; path=/"])]
  where jsid = fromMaybe "dummy" $
                 lookup "Cookie" (requestHeaders req) >>=
                 lookup "JSESSIONID" . parseCookies

hsETag :: ByteString -> [Header]
hsETag etag = [("ETag", etag)]

hsCommon :: Request -> [Header]
hsCommon req = hsCookie req
            ++ hsAC req

ok :: [Header] -> Builder -> Response
ok = ResponseBuilder statusOK

jsOk :: Request -> Builder -> Response
jsOk req = ok (hsJavascript ++ hsCommon req)

sockjsOk :: Request -> SockjsMessage -> Response
sockjsOk req = jsOk req . newline . renderSockjs

notFound :: [Header] -> Response
notFound hs = ResponseBuilder statusNotFound hs mempty

notModified :: Response
notModified = ResponseBuilder statusNotModified [] mempty

noContent :: Request -> Response
noContent req = ResponseBuilder statusNoContent (hsPlain ++ hsCommon req) mempty

serverError :: ByteString -> Response
serverError msg = ResponseBuilder statusServerError [] (B.fromByteString msg)

optionsResponse :: Request -> Response
optionsResponse req = ResponseBuilder statusNoContent
                      (("Allow", "OPTIONS, POST") : (hsCache ++ hsCommon req))
                      mempty

chunkingTestResponse :: Request -> Response
chunkingTestResponse req = ResponseEnumerator enum
  where
    enum f = run_ $ chunkings $$ iter
      where
        iter = f statusOK (hsJavascript ++ hsAC req)
        prelude = E.enumChunks ["h\n", S.replicate 2048 ' ' `mappend` "h\n"]
                  $= E.chunking
        step = E.enumSingle "h\n" $= E.chunking
        chunkings = foldl (\e ms -> e >==> E.ioEnum (threadDelay $ ms*1000) >==> step)
                          prelude
                          [5, 25, 125, 625, 3125]

iframeResponse :: Request -> Response
iframeResponse req =
    case lookup "If-None-Match" (requestHeaders req) of
        Just s | s==hashed -> notModified
        _ -> ok (  hsHtml
                ++ hsCache
                ++ hsETag hashed
                )
                (B.fromLazyByteString content)
  where
    content = "<!DOCTYPE html>\n\
\<html>\n\
\<head>\n\
\  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
\  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
\  <script>\n\
\    document.domain = document.domain;\n\
\    _sockjs_onload = function(){SockJS.bootstrap_iframe();};\n\
\  </script>\n\
\  <script src=\"/static/sockjs.js\"></script>\n\
\</head>\n\
\<body>\n\
\  <h2>Don't panic!</h2>\n\
\  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
\</body>\n\
\</html>"
    hashed = toStrict $ Binary.encode $ md5 content

-- | get messages application.
downstream :: MVar SessionMap
           -> SessionId
           -> WSLite () -- the app to run if not exists.
           -> Int64 -- response size limit, when exceeded, close connection, client will reconnect automitically.
           -> (forall a. Enumerator ByteString IO a) -- prelude data to send.
           -> [Header] -- extra response headers, e.g. content-type.
           -> (Builder -> Builder) -- wraper for response message.
           -> Application
downstream state sid app rsplimit prelude headers wrap req = return $ ResponseEnumerator $ \f -> do
    sess <- getOrNewSession state sid app
    closed' <- readIORef (closed sess)
    liftIO $ putStrLn $ "closed:" ++ show closed'
    if closed'
      then run_ $ (prelude >==> enumMsg (SockjsClose 3000 "Go away!"))
               $= E.chunking
               $$ header f
      else tryModifyMVar (inited sess) 
              -- fail to hold the lock
              ( run_ $ (prelude >==> enumMsg (SockjsClose 2010 "Another connection still open"))
                    $= E.chunking
                    $$ header f
              )
              -- got the lock
              (\inited' -> do
                  cancelTimer sess
                  let open = if inited'
                               then returnI
                               else enumMsg SockjsOpen
                      body = prelude
                             >==> open
                             >==> messages (outChan sess)
                             >==> enumMsg (SockjsClose 3000 "Go away!")
                      iter = body
                           $= E.limit rsplimit
                           $= E.chunking
                           $$ header f
                  -- when write to socket failed, means connection lost, notify inner app.
                  r <- run_ iter `catch` (\e ->
                           atomically (writeTChan (inChan sess) EOF) >>
                           throw (e::SomeException) )
                  startTimer sess
                  return (True, r)
              )
  where
    renderBS = B.toByteString . wrap . renderSockjs
    enumMsg = E.enumChunks . (:[]) . renderBS
    header f = f statusOK (headers ++ hsCommon req ++ hsNoCache)

    messages :: StreamChan ByteString -> Enumerator ByteString IO b
    messages ch = E.enumStreamChanContents ch
               $= EL.map (renderBS . SockjsData)

-- | send message applicatin.
upstream :: MVar SessionMap
         -> SessionId
         -> (L.ByteString -> L.ByteString) -- parse payload from request body.
         -> Response -- success response.
         -> Application
upstream state sid parse successResponse req =
    M.lookup sid <$> liftIO (readMVar state) >>=
    maybe (return $ notFound $ hsCookie req)
          (\sess -> do
              closed' <- liftIO $ readIORef $ closed sess
              if closed'
                then return $ sockjsOk req $ SockjsClose 3000 "Go away!"
                else do
                       payload <- parse . L.fromChunks <$> EL.consume
                       if L.null payload
                         then return $ serverError "Payload expected."
                         else case JSON.decode payload of
                                  Just (SockjsRequest xs) -> do
                                      liftIO $ atomically $ writeTChan (inChan sess) (Chunks xs)
                                      return successResponse
                                  Nothing -> return $ serverError "Broken JSON encoding."
          )

-- }}}

-- session utils {{{

type SessionId = Text

data Session = Session
  { sessionId :: SessionId
  , inChan :: StreamChan ByteString -- | channel for receiving data.
  , outChan :: StreamChan ByteString -- | channel for sending data.
  , closed :: IORef Bool -- | closed or not.
  , inited :: MVar Bool -- | inited or not, also as a lock to prevent multiple receiving connections for a session.
  , mainThread :: ThreadId -- | thread running websocket application.
  , timer :: IORef (Maybe TimerId) -- | a timer to close session.
  }

type SessionMap = Map SessionId Session

newSockjsState :: IO (MVar SessionMap)
newSockjsState = newMVar M.empty

tryModifyMVar :: MVar a -> IO b -> (a -> IO (a, b)) -> IO b
tryModifyMVar m ac f =
    mask $ \restore -> do
        ma <- tryTakeMVar m
        case ma of
            Nothing -> restore ac
            Just a -> do
                (a', b) <- restore (f a) `onException` putMVar m a
                putMVar m a'
                return b

-- | create session if not exists.
getOrNewSession :: MVar SessionMap
                -> SessionId
                -> WSLite ()
                -> IO Session
getOrNewSession state sid app = modifyMVar state $ \sm ->
    case M.lookup sid sm of
        Just old -> return (sm, old)
        Nothing -> do
            putStrLn $ "new session "++show sid
            in_ <- newTChanIO
            out <- newTChanIO
            inited' <- newMVar False
            closed' <- newIORef False
            timer' <- newIORef Nothing
            thread <- forkIO $ do
                -- TODO handle heart beat correctly
                -- runWSLite in_ out (startHBThread >> app)
                runWSLite in_ out app
                M.lookup sid <$> readMVar state >>=
                  maybe (return $ error "impossible:session disappeared before close!")
                        (closeSession state)

            let sess = Session
                        { sessionId = sid
                        , inChan = in_
                        , outChan = out
                        , closed = closed'
                        , inited = inited'
                        , timer = timer'
                        , mainThread = thread
                        }
            let sm' = M.insert sid sess sm
            return (sm', sess)

closeSession :: MVar SessionMap -> Session -> IO ()
closeSession state sess = do
    putStrLn $ "close session "++show (sessionId sess)
    writeIORef (closed sess) True
    atomically $ writeTChan (outChan sess) EOF
    threadDelay (5*1000*1000)
    modifyMVar_ state $ return . M.delete (sessionId sess)

-- | start a timer, when timeout close the session
startTimer :: Session -> IO ()
startTimer sess = do
    putStrLn $ "start timer "++show (sessionId sess)
    mt <- readIORef (timer sess)
    case mt of
        Just t -> clearTimeout t
        Nothing -> return ()
    t <- setTimeout (5*1000*1000) $ atomically $ writeTChan (inChan sess) EOF
    writeIORef (timer sess) (Just t)

-- | try to cancel the close-session timer.
cancelTimer :: Session -> IO ()
cancelTimer sess = do
    putStrLn $ "cancel timer "++show (sessionId sess)
    mt <- readIORef (timer sess)
    case mt of
        Just t -> clearTimeout t >> writeIORef (timer sess) Nothing
        Nothing -> return ()

-- }}}

startHBThread :: WSLite ThreadId
startHBThread = do
    sink <- getSink
    liftIO . forkIO . forever $ do
        threadDelay (heartBeatInterval*1000*1000)
        sink $ B.toByteString $ renderSockjs SockjsHeartbeat

sockjsWrapper :: Monad m => Enumeratee ByteString ByteString m a
sockjsWrapper = E.concatMapMaybe f
  where f s = if S.null s
                then Just [] -- ignore empty message.
                else unSockjsRequest <$> decodeValue s

wsApp :: WSLite () -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsApp lite req = do
    WS.acceptRequest req
    WS.sendTextData ("o"::ByteString)
    sink <- WS.getSink
    let sink' = WS.sendSink sink . WS.textData . B.toLazyByteString . renderSockjs . SockjsData . (:[])
        iter = sockjsWrapper =$ iterWSLite lite sink'
    success <- (WS.runIteratee iter >> return True) `WS.catchWsError`
                   ((>> return False) . liftIO . putStrLn . ("uncaught exception: "++) . show)
    when success $ WS.sendTextData $ B.toByteString $ renderSockjs $ SockjsClose 3000 "Go away!"

wsApps :: WSLiteRoute -> WS.Request -> WS.WebSockets WS.Hybi00 ()
wsApps apps req =
    case msum $ map match apps of
        Just ((app, disallows), [_,_,"websocket"])
          | maybe True (notElem "websocket") disallows -> wsApp app req
        _ -> WS.rejectRequest req "Forbidden!"
  where path = decodePathSegments $ WS.requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

waiRoute :: [([Text], Application)] -> Application -> Application
waiRoute routes fallback req =
    fromMaybe (fallback req) $ msum $ map match routes
  where
    path = pathInfo req
    match (prefix, app) = case stripPrefix prefix path of
        Nothing -> Nothing
        Just rest -> Just $ app req{pathInfo=rest}

