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
  , getMessage
  , getMessages
  , streamMessages
  , deliverReq
  , serverError
  ) where

-- imports {{{
import Debug.Trace

import Data.Map (Map)
import qualified Data.Map as M
import Data.Monoid
import Data.List
import Data.Maybe
import Data.Either
import Data.IORef

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L

import Data.Enumerator hiding (map, foldl, mapM)
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B

import Data.Attoparsec.Lazy (parse, maybeResult)
import Data.Aeson

import Network.HTTP.Types
import Network.Wai

import Network.WebSockets hiding (Request, Response, requestHeaders)
import qualified Network.WebSockets as WS
import Network.WebSockets.Emulate

import Types

-- }}}

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
        mempty -> return []
        "close" -> throwWsError ConnectionClosed
        _ -> maybe (throwWsError $ SockjsError "Broken JSON encoding.")
                   return
                   (unSockjsRequest <$> decode msg)

startHBThread :: TextProtocol p => Int -> WebSockets p ThreadId
startHBThread period = do
    sink <- getSink
    liftIO . forkIO . forever $ do
        threadDelay (period*1000*1000)
        sendSinkSockjs sink SockjsHeartbeat

-- }}}

-- enumerator utils {{{
ioEnum :: IO () -> Enumerator a IO b
ioEnum io step = liftIO io >> E.returnI step

enumChunks :: Monad m => [a] -> Enumerator a m b
enumChunks xs = E.checkContinue0 $ \_ f -> f (E.Chunks xs) >>== E.returnI
-- }}}

-- transport utils {{{

deliverReq :: Session -> L.ByteString -> IO ()
deliverReq sess s = (>> putStrLn ("req:"++ show s)) . atomically
                . (writeTChan (inChan sess))
                . Chunks
                . (:[])
                . B.toByteString
                . encodeFrame EmulateProtocol Nothing
                . Frame True BinaryFrame $ s

parseFramePayload :: L.ByteString -> Maybe L.ByteString
parseFramePayload = (framePayload <$>) . maybeResult . parse (decodeFrame EmulateProtocol)

readStreamChan :: StreamChan a -> STM [a]
readStreamChan ch = loop []
  where
    loop acc = do
        e <- isEmptyTChan ch
        if e then return acc
             else do
                 x <- readTChan ch
                 case x of
                     EOF -> throwSTM SockjsReadEOF
                     (Chunks xs) -> loop (xs++acc)

getMessage :: Session -> IO (Maybe L.ByteString)
getMessage sess = do
    stream <- atomically $ readTChan (outChan sess)
    case stream of
        EOF -> return Nothing
        Chunks xs -> return $ (`mappend` "\n") <$> parseFramePayload (L.fromChunks xs)

getMessages :: Session -> IO [SockjsMessage]
getMessages sess = do
    xs <- atomically $ readStreamChan (outChan sess)
    let m = mapM parseFramePayload (map (L.fromChunks . (:[])) xs) >>= mapM decodeSockjs
    case m of
        Nothing -> throw (SockjsError "Internal Encoding Error")
        Just msgs -> return $ concatData msgs
  where
    concatData :: [SockjsMessage] -> [SockjsMessage]
    concatData = foldr combine []
      where
        combine (SockjsData s) (SockjsData s':xs) = SockjsData (s `mappend` s') : xs
        combine x xs = x : xs

streamMessages :: Session -> Enumerator Builder IO b
streamMessages sess = checkContinue0 $ \loop f -> do
    mr <- liftIO $ getMessage sess
    case mr of
        Nothing -> throwError $ SockjsError "Internal Encoding Error"
        -- Left _ -> f $ Chunks $ (renderSockjs $ SockjsClose 3000 "Go away!") : [B.flush]
        Just r -> do
            liftIO $ putStrLn $ "reply:"++show r
            f (Chunks [B.fromLazyByteString r, B.flush]) >>== loop

-- }}}

-- response utils {{{

hsJavascript, hsCookie, hsCache :: Headers
hsJavascript = [("Content-Type", "application/javascript; charset=UTF-8")]
hsCookie     = [("Set-Cookie", "JSESSIONID=dummy; path=/")]
hsCache      = [("Cache-Control", "public; max-age=31536000;")
               ,("Expires", "31536000")]
hsAC :: ByteString -> Headers
hsAC origin  = [("access-control-max-age", "31536000")
               ,("access-control-allow-origin", origin)
               ,("access-control-allow-credentials", "true")]

ok :: Headers -> Builder -> Response
ok hs b = ResponseBuilder statusOK hs b

notFound :: Response
notFound = ResponseBuilder statusNotFound [] mempty

notAllowed :: Response
notAllowed = ResponseBuilder statusNotAllowed [] mempty

noContent :: Response
noContent = ResponseBuilder statusNoContent [] mempty

serverError :: ByteString -> Response
serverError msg = ResponseBuilder statusServerError [] (B.fromByteString msg)

optionsRsp :: ByteString -> Response
optionsRsp origin = ResponseBuilder statusNoContent
                     ( ("Allow", "OPTIONS, POST")
                     : hsCache
                    ++ hsCookie
                    ++ hsAC origin
                     ) mempty

-- }}}

-- sessions {{{

type SessionId = Text
-- type Session = (StreamChan ByteString, StreamChan ByteString)
data SessionStatus = StInit
                   | StClosed
    deriving (Eq)
data Session = Session
  { inChan :: StreamChan ByteString
  , outChan :: StreamChan ByteString
  , status :: IORef SessionStatus
  , mainThread :: ThreadId
  }

type SessionMap = Map SessionId Session

type WSApp p = WS.Request -> WebSockets p ()
type AppRoute p = [([Text], WSApp p)]

-- | create session if not exists, Left -> old session, Right -> new session.
getOrCreateSession :: MVar SessionMap -> SessionId -> WSApp EmulateProtocol -> Request -> IO (Bool, Session)
getOrCreateSession msm sid app req = modifyMVar msm $ \sm ->
    case M.lookup sid sm of
        Just old -> return (sm, (True, old))
        Nothing -> do
            putStrLn "new session"
            inChan <- newTChanIO
            outChan <- newTChanIO
            st <- newIORef StInit
            thread <- forkIO $ do
                let req' = RequestHttpPart (rawPathInfo req) (requestHeaders req)
                runEmulator inChan outChan req' app
                writeIORef st StClosed
                threadDelay (5*1000*1000)
                modifyMVar_ msm $ return . M.delete sid
            let sm' = M.insert sid sess sm
                sess = Session inChan outChan st thread
            return (sm', (False, sess))

getSession :: MVar SessionMap -> SessionId -> IO (Maybe Session)
getSession msm sid = M.lookup sid <$> readMVar msm

-- }}}

-- main sockjs application {{{

sockjsRoute :: MVar SessionMap -> AppRoute EmulateProtocol -> Application
sockjsRoute msm apps req = case (requestMethod req, msum (map match apps)) of
    ("OPTIONS", Just _) -> return $ optionsRsp $ fromMaybe "*" $ lookup "Origin" (requestHeaders req)

    ("POST", Just (app, path)) -> case path of
        ["chunking_test"] -> return chunkingTest

        [_, sid, path'] -> case path' of
            "xhr" -> liftIO $ do
                (_, sess) <- getOrCreateSession msm sid app req
                msgs <- getMessages sess
                let rsp = ok hsJavascript . mconcat . map renderSockjs $ msgs
                -- TODO start lease
                return rsp

            "xhr_send" ->
                liftIO (getSession msm sid) >>=
                maybe (return notFound)
                      (\sess -> do
                          body <- L.fromChunks <$> EL.consume
                          if (L.null body)
                            then return $ serverError "payload expected."
                            else do
                              liftIO $ deliverReq sess body
                              return noContent
                      )

            "xhr_streaming" -> return $ ResponseEnumerator $ \f -> do
                (exists, sess) <- getOrCreateSession msm sid app req
                flip finally (return ()) $ --(atomically (writeTChan (inChan sess) (Chunks ["close"]))) $
                    run_ $ if exists
                        then f statusBadRequest []
                        else do
                            -- read response
                            _ <- S.concat . toChunks <$> liftIO (atomically (readTChan (outChan sess)))
                            -- TODO parse response
                            let iter = f statusOK
                                    [ ("Content-Type", "application/javascript; charset=UTF-8")
                                    , ("Set-Cookie", "JSESSIONID=dummy; path=/")
                                    , ("access-control-allow-origin", "*")
                                    , ("access-control-allow-credentials", "true")
                                    ]
                                prelude = enumChunks
                                    [ B.fromByteString $ S.replicate 2048 'h'
                                    , B.fromByteString "\n", B.flush
                                    ]
                            prelude >==> streamMessages sess $$ iter

            _ -> return notFound
        _ -> return notFound
    _ -> return notFound
  where
    match (prefix, app) = (app, ) <$> stripPrefix prefix (pathInfo req)

    toChunks (Chunks xs) = xs
    toChunks EOF = error "read EOF"

    chunkingTest :: Response
    chunkingTest = ResponseEnumerator enum
      where
        enum f = run_ $ chunkings $$ iter
          where
            iter = f statusOK [ ("Content-Type", "application/javascript; charset=UTF-8")
                              , ("access-control-allow-origin", "*")
                              , ("access-control-allow-credentials", "true")
                              ]
            prelude = enumChunks [ B.fromByteString "h\n", B.flush
                                 , B.fromByteString $ S.replicate 2048 ' '
                                 , B.fromByteString "h\n", B.flush
                                 ]
            step = enumChunks [B.fromByteString "h\n", B.flush]
            chunkings = foldl (\enum ms -> enum >==> ioEnum (threadDelay $ ms*1000) >==> step)
                              prelude
                              [5, 25, 125, 625, 3125]

-- }}}

wsRoute :: AppRoute Hybi00 -> WSApp Hybi00
wsRoute apps req = case msum $ map match apps of
    Just (app, [_,_,"websocket"]) -> app req
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

