{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Apps where

import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Aeson

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Network.WebSockets

import Sockjs

data TestMessage = TestMessage Text

instance FromJSON TestMessage where
    parseJSON (Array xs)
        | [x] <- V.toList xs = TestMessage <$> parseJSON x
    parseJSON _ = fail "parse fail"

instance ToJSON TestMessage where
    toJSON (TestMessage msg) = toJSON [toJSON msg]

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    forever $ do
        (TestMessage msg) <- receiveJSON
        sendJSON (TestMessage msg)

close :: TextProtocol p => Request -> WebSockets p ()
close req = do
    acceptRequest req
    sendTextData ("c[3000,\"Go away!\"]"::Text)

type ServerState p = Map Text (Sink p) 

clientExists :: Protocol p => Text -> ServerState p -> Bool
clientExists name = maybe False (const True) . M.lookup name

chat :: TextProtocol p => MVar (ServerState p) -> Request -> WebSockets p ()
chat state req = do
    acceptRequest req
    sink <- getSink
    ["join", name] <- receiveJSON
    clients <- liftIO $ readMVar state
    case name of
        _   | any ($ name)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendJSON $ TestMessage $
                        "Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty"
            | clientExists name clients ->
                sendJSON $ TestMessage $ "User already exists"
            | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = M.insert name sink s
                    sendSink sink $ jsonData $ TestMessage $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (M.keys s)
                    broadcast (name `mappend` " joined") s'
                    return s'
                talk state name

broadcast :: TextProtocol p => Text -> ServerState p -> IO ()
broadcast message clients =
    mapM_ (flip sendSink (jsonData $ TestMessage message)) $ M.elems clients

talk :: TextProtocol p => MVar (ServerState p) -> Text -> WebSockets p ()
talk state user = flip catchWsError catchDisconnect $ do
    ["talk", msg] <- receiveJSON
    liftIO $ readMVar state >>= broadcast
        (user `mappend` ": " `mappend` msg)
    talk state user
  where
    catchDisconnect e = case fromException e of
        Just ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
            let s' = M.delete user s
            broadcast (user `mappend` " disconnected") s'
            return s'
        _ -> return ()

