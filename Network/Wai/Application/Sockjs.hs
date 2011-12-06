{-# LANGUAGE OverloadedStrings, TupleSections, ViewPatterns, Rank2Types, ExistentialQuantification #-}
module Network.Wai.Application.Sockjs
  ( AppRoute
  , sockjsApp
  , websocketApp
  , httpRoute
  , newSockjsState
  ) where

-- imports {{{

import Prelude hiding (catch)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Maybe
import Data.IORef
import Data.Int (Int64)

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Data.ByteString (ByteString)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Data.Enumerator hiding (map, foldl, mapM, concatMap)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B

import qualified Data.Attoparsec.Lazy as L
import Data.Aeson (json, Value(Array), encode)
import Data.Digest.Pure.MD5 (md5)
import qualified Data.Binary as Binary

import Network.HTTP.Types
import Network.Wai

import Network.WebSockets hiding (Request, Response, requestHeaders)
import qualified Network.WebSockets as WS
import Network.WebSockets.Emulate

import Web.Cookie

import Network.Sockjs.Types
import Network.Sockjs.Timer

-- }}}

responseLimit :: Int64
responseLimit = 128*1024

type AppRoute p = [([Text], (WSApp p, Maybe [Text]))]

sockjsApp :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
sockjsApp msm apps req = case (requestMethod req, matchResult) of
    ("OPTIONS", Just _) -> return $ optionsResponse req

    (m, Just ((app, disallows), path)) -> case (m, path) of

        ("GET", _) | path==[] || path==[""] ->
            return $ ok hsPlain $ B.fromByteString "Welcome to SockJS!\n"
        ("GET", [ isIframe -> True ]) ->
            return $ iframeResponse req
        ("POST", ["chunking_test"]) ->
            return $ chunkingTestResponse req

        (_, [server, sid, path'])
          | maybe False (elem path') disallows ||
            T.null server || T.null sid ||
            T.any (=='.') server || T.any (=='.') sid ->
                return $ notFound []

        ("POST", [_, sid, "xhr"]) ->
            downstream msm 1 sid app returnI hsJavascript newlineL req

        ("POST", [_, sid, "xhr_streaming"]) ->
            let prelude = enumChunks
                  [ B.fromByteString $ S.replicate 2048 'h'
                  , B.fromByteString "\n"
                  , B.flush
                  ]
            in  downstream msm responseLimit sid app prelude hsJavascript newlineL req

        ("GET", [_, sid, "jsonp"]) ->
            let wrap cb s = mconcat [toLazy cb, "(", encode s, ");\r\n"]
            in withCallback $ \cb -> 
                   downstream msm 1 sid app returnI hsJavascript (wrap cb) req

        ("GET", [_, sid, "htmlfile"]) ->
            withCallback $ \cb ->
                downstream msm responseLimit sid app (htmlfile cb) hsHtml wrap req
          where 
            wrap s = mconcat ["<script>\np(", encode s, ");\n</script>\r\n"]
            htmlfile cb = enumChunks
                [ B.fromByteString
                    "<!doctype html>\n\
                    \<html><head>\n\
                    \  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
                    \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
                    \</head><body><h2>Don't panic!</h2>\n\
                    \  <script>\n\
                    \    document.domain = document.domain;\n\
                    \    var c = parent.", B.fromByteString cb, B.fromByteString ";\n\
                    \    c.start();\n\
                    \    function p(d) {c.message(d);};\n\
                    \    window.onload = function() {c.stop();};\n\
                    \  </script>"
                , B.fromByteString $ S.replicate 658 ' ' -- ensure 1024 length
                , B.flush
                ]

        ("GET", [_, sid, "eventsource"]) ->
            let wrap b = mconcat
                  [ "data: "
                  , b
                  , "\r\n\r\n"
                  ]
                prelude = enumChunks [B.fromByteString "\r\n", B.flush]
            in  downstream msm responseLimit sid app prelude hsEventStream wrap req

        ("POST", [_, sid, "xhr_send"]) ->
            upstream msm sid id noContent req

        ("POST", [_, sid, "jsonp_send"]) ->
            upstream msm sid parse (\req' -> ok (hsCookie req') $ B.fromByteString "ok") req
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
        Just (Just cb) | not (S.null cb) -> f cb
        _ -> return $ serverError "\"callback\" parameter required"

    matchResult = msum (map match apps)
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

    isIframe p = T.isPrefixOf "iframe" p && T.isSuffixOf ".html" p

-- enumerator utils {{{

toLazy :: ByteString -> L.ByteString
toLazy = L.fromChunks . (:[])

toStrict :: L.ByteString -> ByteString
toStrict = S.concat . L.toChunks

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb m1 m2 = do
    b <- mb
    if b then m1 else m2

ioEnum :: IO () -> Enumerator a IO b
ioEnum io step = liftIO io >> E.returnI step

enumChunks :: Monad m => [a] -> Enumerator a m b
enumChunks xs = E.checkContinue0 $ \_ f -> f (E.Chunks xs) >>== E.returnI
-- }}}

-- response utils {{{

newlineL :: L.ByteString -> L.ByteString
newlineL = (`mappend` "\n")

newlineB :: Builder -> Builder
newlineB = (`mappend` B.fromByteString "\n")

hsJavascript, hsPlain, hsHtml, hsEventStream, hsCache, hsNoCache :: Headers
hsJavascript  = [("Content-Type", "application/javascript; charset=UTF-8")]
hsPlain       = [("Content-Type", "text/plain; charset=UTF-8")]
hsHtml        = [("Content-Type", "text/html; charset=UTF-8")]
hsEventStream = [("Content-Type", "text/event-stream; charset=UTF-8")]
hsCache       = [("Cache-Control", "public; max-age=31536000;")
                ,("Expires", "31536000")]
hsNoCache     = [("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")]
hsAC :: Request -> Headers
hsAC req = [("access-control-max-age", "31536000")
           ,("access-control-allow-origin", origin)
           ,("access-control-allow-credentials", "true")]
  where origin = fromMaybe "*" $ lookup "Origin" $ requestHeaders req

hsCookie :: Request -> Headers
hsCookie req = [("Set-Cookie", mconcat ["JSESSIONID=", jsid, "; path=/"])]
  where jsid = fromMaybe "dummy" $
                 lookup "Cookie" (requestHeaders req) >>=
                 lookup "JSESSIONID" . parseCookies

hsETag :: ByteString -> Headers
hsETag etag = [("ETag", etag)]

hsCommon :: Request -> Headers
hsCommon req = hsCookie req
            ++ hsAC req

ok :: Headers -> Builder -> Response
ok = ResponseBuilder statusOK

jsOk :: Request -> Builder -> Response
jsOk req = ok (hsJavascript ++ hsCommon req)

sockjsOk :: Request -> SockjsMessage -> Response
sockjsOk req = jsOk req . newlineB . renderSockjs

notFound :: Headers -> Response
notFound hs = ResponseBuilder statusNotFound hs mempty

notModified :: Response
notModified = ResponseBuilder statusNotModified [] mempty

notAllowed :: Request -> Response
notAllowed req = ResponseBuilder statusNotAllowed (hsCommon req) mempty

noContent :: Request -> Response
noContent req = ResponseBuilder statusNoContent (hsPlain ++ hsCommon req) mempty

serverError :: ByteString -> Response
serverError msg = ResponseBuilder statusServerError [] (B.fromByteString msg)

optionsResponse :: Request -> Response
optionsResponse req = ResponseBuilder statusNoContent
                     ( ("Allow", "OPTIONS, POST")
                     : hsCache
                    ++ hsCommon req
                     ) mempty

chunkingTestResponse :: Request -> Response
chunkingTestResponse req = ResponseEnumerator enum
  where
    enum f = run_ $ chunkings $$ iter
      where
        iter = f statusOK (hsJavascript ++ hsAC req)
        prelude = enumChunks [ B.fromByteString "h\n", B.flush
                             , B.fromByteString $ S.replicate 2048 ' '
                             , B.fromByteString "h\n", B.flush
                             ]
        step = enumChunks [B.fromByteString "h\n", B.flush]
        chunkings = foldl (\e ms -> e >==> ioEnum (threadDelay $ ms*1000) >==> step)
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

-- | pass data from outChan to client streamlined.
downstream :: MVar SessionMap
           -> Int64 -- response size limit, when exceeded, close connection, client will reconnect automitically.
           -> SessionId
           -> WSApp EmulateProtocol -- the websocket app to run if not exists.
           -> (forall a. Enumerator Builder IO a) -- prelude data to send.
           -> Headers -- extra response headers, e.g. content-type.
           -> (L.ByteString -> L.ByteString) -- wraper for response message.
           -> Application
downstream msm rsplimit sid app prelude headers wrap req = return $ ResponseEnumerator $ \f -> do
    sess <- getOrNewSession msm sid app req
    closed' <- readIORef (closed sess)
    liftIO $ putStrLn $ "closed:" ++ show closed'
    if closed'
      then run_ $ prelude >==> enumChunks [wrapB $ renderSockjs $ SockjsClose 3000 "Go away!", B.flush]
               $$ header f
      else tryModifyMVar (inited sess) 
              ( run_ $ prelude >==> enumChunks [wrapB $ renderSockjs $ SockjsClose 2010 "Another connection still open", B.flush]
                    $$ header f
              )
              (\inited' -> do
                  if inited'
                    then cancelTimer sess
                    else do -- ignore response
                      _ <- atomically $ readTChan $ outChan sess
                      return ()
                  let body = prelude >==> messages (outChan sess)
                  r <- run_ (body $$ header f)
                       `catch` (\e -> atomically (writeTChan (inChan sess) EOF) >> throw (e::SomeException))
                  startTimer sess
                  return (True, r)
              )
  where
    header f = f statusOK (headers ++ hsCommon req ++ hsNoCache)
    wrapB = B.fromLazyByteString . wrap . B.toLazyByteString

    messages :: StreamChan ByteString -> Enumerator Builder IO b
    messages ch = enumChan ch
               $= EL.map (wrap . parseFramePayload . toLazy)
               $= limit rsplimit
               $= EL.concatMap ((:[B.flush]) . B.fromLazyByteString)

upstream :: MVar SessionMap
         -> SessionId
         -> (L.ByteString -> L.ByteString) -- parse payload from request body.
         -> (Request -> Response) -- success response.
         -> Application
upstream msm sid parse successResponse req =
    M.lookup sid <$> liftIO (readMVar msm) >>=
    maybe (return $ notFound $ hsCookie req)
          (\sess ->
              ifM (liftIO $ readIORef $ closed sess)
                  (return $ sockjsOk req $ SockjsClose 3000 "Go away!")
                  ( do
                      msg <- parse . L.fromChunks <$> EL.consume
                      if L.null msg
                        then return $ serverError "Payload expected."
                        else -- validate json format.
                            case L.parse json msg of
                              L.Done _ (Array _) -> do
                                  liftIO $ passRequest (inChan sess) msg
                                  return $ successResponse req
                              _ -> return $ serverError "Broken JSON encoding."
                  )
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

type WSApp p = WS.Request -> WebSockets p ()

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
                -> WSApp EmulateProtocol
                -> Request
                -> IO Session
getOrNewSession msm sid app req = modifyMVar msm $ \sm ->
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
                let req' = RequestHttpPart (rawPathInfo req) (requestHeaders req)
                runEmulator in_ out req' (\r -> app r `catchWsError` const (return ()))
                M.lookup sid <$> readMVar msm >>=
                  maybe (return $ error "impossible:session disappeared before close!")
                        (closeSession msm)

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
closeSession msm sess = do
    putStrLn $ "close session "++show (sessionId sess)
    writeIORef (closed sess) True
    atomically $ writeTChan (outChan sess) EOF
    threadDelay (5*1000*1000)
    modifyMVar_ msm $ return . M.delete (sessionId sess)

startTimer :: Session -> IO ()
startTimer sess = do
    putStrLn $ "start timer "++show (sessionId sess)
    mt <- readIORef (timer sess)
    case mt of
        Just t -> clearTimeout t
        Nothing -> return ()
    t <- setTimeout (5*1000*1000) $ atomically $ writeTChan (inChan sess) EOF
    writeIORef (timer sess) (Just t)

cancelTimer :: Session -> IO ()
cancelTimer sess = do
    putStrLn $ "cancel timer "++show (sessionId sess)
    mt <- readIORef (timer sess)
    case mt of
        Just t -> clearTimeout t >> writeIORef (timer sess) Nothing
        Nothing -> return ()

-- }}}

-- transport utils {{{

passRequest :: StreamChan ByteString -> L.ByteString -> IO ()
passRequest ch =
    atomically
  . writeTChan ch
  . return
  . B.toByteString
  . encodeFrame EmulateProtocol Nothing
  . Frame True BinaryFrame

parseFramePayload :: L.ByteString -> L.ByteString
parseFramePayload = fromMaybe (error "Internal Encoding Error [downstream]") .
                        (framePayload <$>) . L.maybeResult . L.parse (decodeFrame EmulateProtocol)

limit :: Monad m => Int64 -> Enumeratee L.ByteString L.ByteString m b
limit n step | n <= 0 = return step
limit n (Continue k) = continue loop where
    loop (Chunks []) = continue loop
    loop (Chunks xs) = iter where
        len = L.length (L.concat xs)
        iter = if len <= n
            then k (Chunks xs) >>== limit (n - len)
            else k (Chunks xs) >>== (`E.yield` Chunks [])
    loop EOF = k EOF >>== (`E.yield` EOF)
limit _ step = return step

-- }}}

websocketApp :: AppRoute Hybi00 -> WSApp Hybi00
websocketApp apps req = case msum $ map match apps of
    Just ((app, disallows), [_,_,"websocket"])
      | maybe True (notElem "websocket") disallows -> app req
    _ -> WS.rejectRequest req "Forbidden!"
  where path = decodePathSegments $ WS.requestPath req
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

httpRoute :: [([Text], Application)] -> Application -> Application
httpRoute routes fallback req =
    fromMaybe (fallback req) $ msum $ map match routes
  where
    path = pathInfo req
    match (prefix, app) = case stripPrefix prefix path of
        Nothing -> Nothing
        Just rest -> Just $ app req{pathInfo=rest}

