{-# LANGUAGE OverloadedStrings #-}
module Chat where

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.State

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

data Client = Client
  { identity :: Identity
  , transport :: Transport
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

numClients :: ServerM Int
numClients = M.size <$> getState

addClient :: Identity -> ServerM Transport
addClient identity = do
    client <- liftIO $ newClient identity
    modifyState (M.insert identity client)
    return $ transport client

getClient :: Identity -> ServerM (Maybe Client)
getClient identity = M.lookup identity <$> getState

broadcast :: Message -> ServerM ()
broadcast msg = do
    chans <- map snd . map transport . M.elems <$> getState
    mapM_ (liftSTM . flip writeTChan msg) chans

echo :: Message -> Client -> ServerM ()
echo msg c = do
    let tranRsp = snd . transport $ c
    liftSTM $ writeTChan tranRsp msg

recv :: Client -> ServerM Message
recv c = liftSTM $ readTChan (snd . transport $ c)

testClient :: Identity -> Message -> ServerM ()
testClient identity msg = do
    (chanReq, chanRsp) <- addClient identity
    liftIO $ forkIO $ forever $ do
        s <- atomically (readTChan chanRsp)
        T.putStrLn $ T.concat [identity, ":", s]
    broadcast msg

main = do
    runServerM $ forM_ [1..10] $ \i -> testClient (T.pack $ "id"++show i) (T.pack $ "hello from"++show i)
    return ()
