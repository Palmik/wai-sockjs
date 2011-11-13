{-# LANGUAGE OverloadedStrings #-}
module Chat where

import System.Posix
import Data.IORef

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Control.Monad.State hiding (join)

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = readTVar v >>= writeTVar v . f

liftSTM :: STM a -> ServerM a
liftSTM = liftIO . atomically

getState :: ServerM ServerState
getState = get >>= liftSTM . readTVar

modifyState :: (ServerState -> ServerState) -> ServerM ()
modifyState f = get >>= liftSTM . flip modifyTVar f

type Identity = Text
type Message = Text

type Transport = (TChan Message, TChan Message)

data Status = StatusInit
            | StatusReceiving
            | StatusClosed

data Client = Client
  { identity :: Identity
  , transport :: Transport
  , status :: IORef Status
  , timestamp :: IORef EpochTime
  }

type Clients = Map Identity Client
type ServerState = Map Identity Client
type TServerState = TVar ServerState

type ServerM = StateT TServerState IO

runServerM = evalStateT

newServerState :: IO TServerState
newServerState = newTVarIO M.empty

newClient :: Identity -> IO Client
newClient identity = Client <$> pure identity
                            <*> ( (,) <$> newTChanIO <*> newTChanIO)
                            <*> newIORef StatusInit
                            <*> (epochTime >>= newIORef)

numClients :: ServerM Int
numClients = M.size <$> getState

join :: Identity -> ServerM Client
join identity = do
    client <- liftIO $ newClient identity
    modifyState (M.insert identity client)
    return $ client

getClient :: Identity -> ServerM (Maybe Client)
getClient identity = M.lookup identity <$> getState

broadcast :: Message -> ServerM ()
broadcast msg = do
    chans <- map snd . map transport . M.elems <$> getState
    mapM_ (liftSTM . flip writeTChan msg) chans

echo :: Client -> Message -> ServerM ()
echo c msg = do
    let tranRsp = snd . transport $ c
    liftSTM $ writeTChan tranRsp msg

recv :: Client -> ServerM Message
recv c = liftSTM $ readTChan (snd . transport $ c)

recvOrJoin :: Identity -> ServerM (Either Client Message)
recvOrJoin identity = do
    mc <- getClient identity
    case mc of
        Nothing -> Left <$> join identity
        Just c -> Right <$> recv c

testClient :: Identity -> Message -> ServerM ()
testClient identity msg = do
    chanRsp <- snd . transport <$> join identity
    liftIO $ forkIO $ forever $ do
        s <- atomically (readTChan chanRsp)
        T.putStrLn $ T.concat [identity, ":", s]
    broadcast msg

main = do
    runServerM $ forM_ [1..10] $ \i -> testClient (T.pack $ "id"++show i) (T.pack $ "hello from"++show i)
    return ()
