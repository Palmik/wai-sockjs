{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Apps where

import Data.Char
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Map (Map)
import qualified Data.Map as M

import Control.Exception
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Network.WebSockets

import qualified Blaze.ByteString.Builder as B

import Types
import Sockjs

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    sendSockjs SockjsOpen
    forever $ do
        msg <- receiveSockjs
        when (not $ null msg) $ sendSockjs $ SockjsData (msg::[ByteString])

close :: TextProtocol p => Request -> WebSockets p ()
close req = do
    acceptRequest req
    sendSockjs SockjsOpen
    sendSockjs $ SockjsClose 3000 "Go away!"

type ServerState p = Map Text (Sink p) 

clientExists :: Protocol p => Text -> ServerState p -> Bool
clientExists name = maybe False (const True) . M.lookup name

chat :: TextProtocol p => MVar (ServerState p) -> Request -> WebSockets p ()
chat state req = do
    acceptRequest req
    sendSockjs SockjsOpen
    sink <- getSink
    msg <- receiveData
    clients <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `T.isPrefixOf` msg) ->
                sendTextData ("Wrong Annoucement!"::Text)
            | any ($ name)
                [T.null, T.any isPunctuation, T.any isSpace] ->
                    sendTextData $
                        "Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        ("cannot be empty"::Text)
            | clientExists name clients ->
                sendTextData ("User already exists"::Text)
            | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = M.insert name sink s
                    sendSink sink $ textData $
                        "Welcome! Users: " `mappend`
                        T.intercalate ", " (M.keys s)
                    broadcast (name `mappend` " joined") s'
                    return s'
                talk state name
          where
            prefix = "Hi! I'm "
            name = T.drop (T.length prefix) msg

broadcast :: TextProtocol p => Text -> ServerState p -> IO ()
broadcast message clients =
    mapM_ (flip sendSink (textData message)) $ M.elems clients

talk :: TextProtocol p => MVar (ServerState p) -> Text -> WebSockets p ()
talk state user = flip catchWsError catchDisconnect $ do
    msg <- receiveData
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

