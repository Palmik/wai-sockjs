{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Sockjs
  ( AppRoute
  , sockjsRoute
  , wsRoute
  , httpRoute
  , sendSockjs
  , sendSockjsData
  , sockjsData
  , sendSinkSockjs
  , receiveSockjs
  , startHBThread
  , getPayload
  , getMessages
  , streamMessages
  , passRequest
  , serverError
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
import Data.Aeson

import Network.HTTP.Types
import Network.Wai

import Network.WebSockets hiding (Request, Response, requestHeaders)
import qualified Network.WebSockets as WS
import Network.WebSockets.Emulate

import Web.Cookie

import Types
import Timer

-- }}}

streamResponseSize :: Int64
streamResponseSize = 4096
heartBeatInterval :: Int
heartBeatInterval = 25

-- sockjs websocket utils {{{

sendSockjs :: TextProtocol p => SockjsMessage -> WebSockets p ()
sendSockjs = sendTextData . B.toLazyByteString . renderSockjs

sendSockjsData :: TextProtocol p => ByteString -> WebSockets p ()
sendSockjsData = sendSockjs . SockjsData . (:[])

sockjsData :: (TextProtocol p, WebSocketsData a) => a -> Message p
sockjsData = textData . B.toLazyByteString . renderSockjs . SockjsData . (:[]) . mconcat . L.toChunks . toLazyByteString

sendSinkSockjs :: TextProtocol p => Sink p -> SockjsMessage -> IO ()
sendSinkSockjs sink = sendSink sink . textData . B.toLazyByteString . renderSockjs

receiveSockjs :: (TextProtocol p, FromJSON a, Monoid a) => WebSockets p a
receiveSockjs = mconcat <$> receiveSockjs'

receiveSockjs' :: (TextProtocol p, FromJSON a) => WebSockets p [a]
receiveSockjs' = do
    msg <- receiveData
    case msg of
        "" -> receiveSockjs'
        _ -> maybe (throwWsError (SockjsError "Broken JSON encoding [receive]."))
                   return
                   (unSockjsRequest <$> decodeValue msg)

startHBThread :: TextProtocol p => WebSockets p ThreadId
startHBThread = do
    sink <- getSink
    liftIO . forkIO . forever $ do
        threadDelay (heartBeatInterval*1000*1000)
        sendSinkSockjs sink SockjsHeartbeat

-- }}}

-- enumerator utils {{{

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

newline :: Builder -> Builder
newline = (`mappend` B.fromByteString "\n")

hsJavascript, hsPlain, hsHtml, hsCache :: Headers
hsJavascript  = [("Content-Type", "application/javascript; charset=UTF-8")]
hsPlain       = [("Content-Type", "text/plain; charset=UTF-8")]
hsHtml        = [("Content-Type", "text/html; charset=UTF-8")]
hsCache       = [("Cache-Control", "public; max-age=31536000;")
                ,("Expires", "31536000")]
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
ok hs b = ResponseBuilder statusOK hs b

jsOk :: Request -> Builder -> Response
jsOk req = ok (hsJavascript ++ hsCommon req)

sockjsOk :: Request -> SockjsMessage -> Response
sockjsOk req = jsOk req . newline . renderSockjs

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

optionsRsp :: Request -> Response
optionsRsp req = ResponseBuilder statusNoContent
                     ( ("Allow", "OPTIONS, POST")
                     : hsCache
                    ++ hsCommon req
                     ) mempty

iframeRsp :: Request -> Response
iframeRsp req =
    case lookup "If-None-Match" (requestHeaders req) of
        Just s | s==md5 -> notModified
        _ -> ok (  hsHtml
                ++ hsCache
                ++ hsETag md5
                )
                (B.fromByteString content)
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
    md5 = "test"
-- }}}

-- sessions {{{

type SessionId = Text
data SessionStatus = StStreaming
                   | StClosing
                   | StClosed
    deriving (Eq, Show)
data Session = Session
  { sessionId :: SessionId
  , inChan :: StreamChan ByteString
  , outChan :: StreamChan ByteString
  , closed :: IORef Bool
  , inited :: MVar Bool
  , mainThread :: ThreadId
  , timer :: IORef (Maybe TimerId)
  }

type SessionMap = Map SessionId Session

type WSApp p = WS.Request -> WebSockets p ()
type AppRoute p = [([Text], (WSApp p, Maybe [Text]))]

tryModifyMVar :: MVar a -> IO b -> (a -> IO (a, b)) -> IO b
tryModifyMVar m ac f = do
    mask $ \restore -> do
        ma <- tryTakeMVar m
        case ma of
            Nothing -> restore ac
            Just a -> do
                (a', b) <- restore (f a) `onException` putMVar m a
                putMVar m a'
                return b

-- | create session if not exists, Left -> old session, Right -> new session.
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
                runEmulator in_ out req' (\r -> app r `catchWsError` (const $ return ()))
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

passRequest :: Session -> L.ByteString -> IO ()
passRequest sess s = atomically
                . (writeTChan (inChan sess))
                . Chunks
                . (:[])
                . B.toByteString
                . encodeFrame EmulateProtocol Nothing
                . Frame True BinaryFrame $ s

parseFramePayload :: L.ByteString -> Maybe L.ByteString
parseFramePayload = (framePayload <$>) . L.maybeResult . L.parse (decodeFrame EmulateProtocol)

getContentsChan :: StreamChan a -> STM [a]
getContentsChan ch = loop []
  where
    loop acc = do
        e <- isEmptyTChan ch
        if e then return acc
             else do
                 x <- readTChan ch
                 case x of
                     EOF -> return acc
                     (Chunks xs) -> loop (xs++acc)

getMessages :: StreamChan ByteString -> IO [SockjsMessage]
getMessages ch = do
    xs <- atomically $
            ifM (isEmptyTChan ch)
                (streamToList <$> readTChan ch)
                (getContentsChan ch)
    if null xs
      then return []
      else do
        let m = mapM parseFramePayload (map toLazy xs) >>= mapM decodeSockjs
        case m of
            Nothing -> throw (SockjsError "Internal Encoding Error Downstream")
            Just msgs -> return $ concatData msgs
  where
    toLazy = L.fromChunks . (:[])
    concatData :: [SockjsMessage] -> [SockjsMessage]
    concatData = foldr combine [] . reverse
      where
        combine (SockjsData s) (SockjsData s':xs) = SockjsData (s ++ s') : xs
        combine x xs = x : xs

    streamToList (Chunks xs) = xs
    streamToList EOF = []

withSize :: Monad m
            => (Int64
             -> (Int64 -> Enumerator a m b)
             -> (Stream a -> Iteratee a m b)
             -> Iteratee a m b)
            -> Enumerator a m b
withSize inner = loop 0 where
    loop acc (Continue k) = inner acc loop k
    loop _ step = returnI step

getPayload :: StreamChan ByteString -> IO (Maybe L.ByteString)
getPayload ch = do
    stream <- atomically $ readTChan ch
    case stream of
        EOF -> return Nothing
        Chunks xs -> return $ parseFramePayload (L.fromChunks xs)

streamMessages :: StreamChan ByteString -> Int64 -> Enumerator Builder IO b
streamMessages ch maxsize =
    withSize $ \acc loop f -> do
        msgs <- liftIO $ getMessages ch
        liftIO $ print msgs
        if null msgs
          -- then f $ Chunks [newline $ renderSockjs $ SockjsClose 3000 "Go away!", B.flush]
          then f $ Chunks [newline $ renderSockjs $ SockjsClose 3000 "Go away!", B.flush]
          else do
            let strs = map (B.toLazyByteString . newline . renderSockjs) msgs
            let iter = f $ Chunks $ concatMap ((:[B.flush]) . B.fromLazyByteString) strs
            let acc' = acc + sum (map L.length strs)
            if acc' >= maxsize
              then iter
              else iter >>== loop acc'

-- }}}

-- main sockjs application {{{

sockjsRoute :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
sockjsRoute msm apps req = case (requestMethod req, matchResult) of
    ("OPTIONS", Just _) -> return $ optionsRsp req

    (m, Just ((app, disallows), path)) -> case (m, path) of

        ("GET", []) -> return $ ok hsPlain $ B.fromByteString "Welcome to SockJS!\n"
        ("GET", [""]) -> return $ ok hsPlain $ B.fromByteString "Welcome to SockJS!\n"
        ("GET", [piece]) | isIframe piece -> return $ iframeRsp req

        ("POST", ["chunking_test"]) -> return chunkingTest

        ("POST", [server, sid, path'])
          | T.null server || T.null sid || T.any (=='.') server || T.any (=='.') sid
              -> return $ notFound []
          | maybe True (not . elem path') disallows -> case path' of
                "xhr"           -> return $ xhrStreaming 0 sid app
                "xhr_streaming" -> return $ xhrStreaming streamResponseSize sid app
                "xhr_send" ->
                    liftIO (M.lookup sid <$> readMVar msm) >>=
                    maybe (return $ notFound $ hsCookie req)
                          (\sess ->
                              ifM (liftIO $ readIORef $ closed sess)
                                  (return . sockjsOk req $ SockjsClose 3000 "Go away!")
                                  ( do
                                      body <- L.fromChunks <$> EL.consume
                                      if (L.null body)
                                          then return $ serverError "Payload expected."
                                          else case L.parse json body of
                                              L.Done _ (Array _) -> do
                                                  liftIO $ passRequest sess body
                                                  return $ noContent req
                                              _ -> return $ serverError "Broken JSON encoding."
                                  )
                          )
                _ -> return $ notFound []
        _ -> return $ notFound []
    _ -> return $ notFound []
  where
    matchResult = msum (map match apps)
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

    isIframe p = T.isPrefixOf "iframe" p && T.isSuffixOf ".html" p

    chunkingTest :: Response
    chunkingTest = ResponseEnumerator enum
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

    xhrStreaming :: Int64 -> SessionId -> WSApp EmulateProtocol -> Response
    xhrStreaming rspsize sid app = ResponseEnumerator $ \f -> do
        sess <- getOrNewSession msm sid app req
        closed' <- readIORef (closed sess)
        if closed'
          then run_ $ enumChunks [newline $ renderSockjs $ SockjsClose 3000 "Go away!"]
                   $$ f statusOK (hsJavascript ++ hsCommon req)
          else tryModifyMVar (inited sess) 
                  ( run_ $ enumChunks [newline $ renderSockjs $ SockjsClose 2010 "Another connection still open"]
                        $$ f statusOK (hsJavascript ++ hsCommon req)
                  )
                  (\inited' -> do
                      if inited'
                        then cancelTimer sess
                        else do -- ignore response
                          _ <- atomically $ readTChan $ outChan sess
                          return ()
                      let enum = if not inited' && rspsize>0
                                    then prelude >==> streamMessages (outChan sess) rspsize
                                    else streamMessages (outChan sess) rspsize
                      r <- run_ (enum $$ header f)
                           `catch` (\e -> atomically (writeTChan (inChan sess) EOF) >> throw (e::SomeException))
                      startTimer sess
                      return (True, r)
                  )
      where
        header f = f statusOK (hsJavascript ++ hsCommon req)

        prelude = enumChunks
            [ newline $ B.fromByteString $ S.replicate 2048 'h'
            , B.flush
            ]
-- }}}

wsRoute :: AppRoute Hybi00 -> WSApp Hybi00
wsRoute apps req = case msum $ map match apps of
    Just ((app, disallows), [_,_,"websocket"])
      | maybe True (not . elem "websocket") disallows -> app req
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

