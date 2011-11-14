import qualified Network.WebSockets as WS
import Data.ByteString (ByteString)
import Data.Enumerator
import Data.Map (Map)
import qualified Data.Map as M
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

runWithChannels :: WS.Protocol p
                => StreamChan ByteString -> StreamChan ByteString -> (WS.Request -> WS.WebSockets p a) -> IO a
runWithChannels inChan outChan ws = do
    r <- run $ enumChan inChan $$
            WS.runWebSocketsWithHandshake WS.defaultWebSocketsOptions ws (iterChan outChan)
    writeChan outChan EOF
    either (error . show) return r

data Sockjs = Sockjs
instance Protocol Sockjs where
    version _ = "sockjs"
    headerVersion _ = "sockjs"
    encodeFrame _ mask (Frame _ _ payload) = payload
    decodeFrame _ = Frame True TextFrame <$> takeByteString
    -- finishRequest _ = 
    implementations = [Sockjs]

type SessionId = ByteString
data Session = Session
  { sessionId :: SessionId
  , inChan :: StreamChan ByteString
  , outChan :: StreamChan ByteString
  }
type SessionMap = Map SessionId Session

ensureSession :: WS.Protocol p => MVar SessionMap -> SessionId -> (WS.Request -> WS.WebSockets p a) -> IO Session
ensureSession msm sid app = modifyMVar msm $ \sm -> do
    case M.lookup sid sm of
        Just old -> return (sm, old)
        Nothing -> do
            new <- Session <$> pure sid 
                           <*> newChan
                           <*> newChan
            let sm' = M.insert sid new sm
            forkIO $ runWithChannels (inChan new) (outChan new) app >> return ()
            return (sm', new)
