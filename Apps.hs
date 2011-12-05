{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Apps where

import Data.Char
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Data.Map (Map)
import qualified Data.Map as M

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent

import Network.WebSockets

import Network.Sockjs.Types
import Network.Sockjs

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    _ <- startHBThread
    sendSockjs SockjsOpen
    forever $ do
        msg <- receiveSockjs
        sendSockjsData msg

close :: TextProtocol p => Request -> WebSockets p ()
close req = do
    acceptRequest req
    sendSockjs SockjsOpen

type ServerState p = Map ByteString (Sink p) 

clientExists :: Protocol p => ByteString -> ServerState p -> Bool
clientExists name = maybe False (const True) . M.lookup name

chat :: TextProtocol p => MVar (ServerState p) -> Request -> WebSockets p ()
chat state req = do
    acceptRequest req
    sendSockjs SockjsOpen
    sink <- getSink
    msg <- receiveSockjs
    clients <- liftIO $ readMVar state
    case msg of
        _   | not (prefix `S.isPrefixOf` msg) ->
                sendSockjsData "Wrong Annoucement!"
            | any ($ name)
                [S.null, S.any isPunctuation, S.any isSpace] ->
                    sendSockjsData $
                        "Name cannot " `mappend`
                        "contain punctuation or whitespace, and " `mappend`
                        "cannot be empty"
            | clientExists name clients ->
                sendSockjsData "User already exists"
            | otherwise -> do
                liftIO $ modifyMVar_ state $ \s -> do
                    let s' = M.insert name sink s
                    sendSink sink $ sockjsData $
                        "Welcome! Users: " `mappend`
                        S.intercalate ", " (M.keys s)
                    broadcast (name `mappend` " joined") s'
                    return s'
                _ <- startHBThread
                talk state name
          where
            prefix = "Hi! I am "
            name = S.drop (S.length prefix) msg

broadcast :: TextProtocol p => ByteString -> ServerState p -> IO ()
broadcast message clients =
    mapM_ (flip sendSink (sockjsData message)) $ M.elems clients

talk :: TextProtocol p => MVar (ServerState p) -> ByteString -> WebSockets p ()
talk state user = flip catchWsError catchDisconnect $ do
    msg <- receiveSockjs
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

