{-# LANGUAGE OverloadedStrings #-}
import Network.WebSockets hiding (fromLazyByteString)
import qualified Network.WebSockets.Protocol as I
import Data.Map (Map)
import qualified Data.Map as M

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Enumerator
import Data.Attoparsec
import Blaze.ByteString.Builder

import Control.Monad.IO.Class
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar

type StreamChan a = Chan (Stream a)

iterChan :: StreamChan a -> Iteratee a IO ()
iterChan ch = continue go
  where
    go EOF = liftIO $ writeChan ch EOF
    go stream = liftIO (writeChan ch stream) >> continue go

enumChan :: StreamChan a -> Enumerator a IO b
enumChan ch = checkContinue0 $ \loop f -> do
    stream <- liftIO $ readChan ch
    f stream >>== loop

runWithChannels :: Protocol p
                => StreamChan ByteString -> StreamChan ByteString -> (Request -> WebSockets p a) -> IO a
runWithChannels inChan outChan ws = do
    r <- run $ enumChan inChan $$
            runWebSockets defaultWebSocketsOptions ws (iterChan outChan)
    writeChan outChan EOF
    either (error . show) return r

data SockjsProtocol = SockjsProtocol
instance Protocol SockjsProtocol where
    version _ = "sockjs"
    headerVersion _ = "sockjs"
    encodeFrame _ mask (Frame _ _ payload) = fromLazyByteString payload
    decodeFrame _ = Frame True TextFrame <$> takeLazyByteString
    finishRequest _ (RequestHttpPart path hs) = return $ Right $ Request path hs response101
    implementations = [SockjsProtocol]
instance TextProtocol SockjsProtocol

type SessionId = ByteString
data Session = Session
  { sessionId :: SessionId
  , inChan :: StreamChan ByteString
  , outChan :: StreamChan ByteString
  }
type SessionMap = Map SessionId Session

type WSApp a = Request -> WebSockets SockjsProtocol a
type AppRoute = [([ByteString], WSApp a)]

newSockjsSession :: MVar SessionMap -> SessionId -> WSApp a -> IO Session
newSockjsSession msm sid app = modifyMVar msm $ \sm -> do
    case M.lookup sid sm of
        Just old -> return (sm, old)
        Nothing -> do
            new <- Session <$> pure sid 
                           <*> newChan
                           <*> newChan
            let sm' = M.insert sid new sm
            forkIO $ runWithChannels (inChan new) (outChan new) app >> return ()
            return (sm', new)


wsApp :: AppRoute -> Request -> WebSockets SockjsProtocol a
wsApp apps req = case msum $ map matchOne apps of
    Just (app, (_:_:"websocket")) -> do
        acceptRequest req
        app req
    _ -> rejectRequest req "Forbidden!"

  where path = S.split '/' (requestPath req)
        match (prefix, app) = (app, ) <$> stripPrefix prefix path

echoApp :: Request -> WebSockets SockjsProtocol a
echoApp req = forever $ do
    msg <- receiveData
    sendTextData (msg::L.ByteString)

