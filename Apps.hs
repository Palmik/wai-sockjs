{-# LANGUAGE OverloadedStrings #-}
module Apps where

import           Control.Exception (fromException)
import           Control.Monad (forever, when, forM_)
import           Control.Monad.IO.Class (liftIO)
import           Control.Applicative
import           Control.Concurrent.MVar

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid (mappend, mconcat)
import           Data.Attoparsec
import           Data.Attoparsec.Char8 (skipSpace)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S

import           Network.WebSockets.Lite

echo :: WSLite ()
echo = forever $ recvBS >>= send

close' :: WSLite ()
close' = return ()

data ChatMessage = ChatJoin ByteString
                 | ChatData ByteString
                 | ChatError ByteString

chatParser :: Parser ChatMessage
chatParser = ChatJoin <$> (string "join" *> skipSpace *> takeByteString)
         <|> ChatData <$> takeByteString

instance UpProtocol ChatMessage where
    decode = parseOnly chatParser

instance DownProtocol ChatMessage where
    encode (ChatData s) = s
    encode (ChatError e) = "error: " `mappend` e
    encode (ChatJoin name) = name `mappend` " joined"

type ChatState = MVar (Map ByteString Sink)

newChatState :: IO ChatState
newChatState = newMVar M.empty

chat :: ChatState -> WSLite ()
chat clients = do
    name <- recvJoin
    sink <- getSink
    exists <- liftIO $ modifyMVar clients $ \cs ->
        case M.lookup name cs of
            Nothing -> return (M.insert name sink cs, False)
            Just _  -> return (cs, True)
    when exists $ fail' "User already exists."

    flip catchError (handleDisconnect name) $ do
        welcome name
        broadcast $ ChatJoin name
        forever $ do
            msg <- recv
            case msg of
                ChatData s -> broadcast $ ChatData $ mconcat [name, ": ", s]
                _ -> fail' "invalid message."
  where
    fail' s = send (ChatError s) >> close
    recvJoin = do msg <- recv
                  case msg of
                      ChatJoin name -> return name
                      _ -> fail' "invalid message."

    broadcast msg = do
        sinks <- M.elems <$> liftIO (readMVar clients)
        forM_ sinks (`sendSink` msg)

    welcome name = do
        users <- filter (/=name) . M.keys <$> liftIO (readMVar clients)
        send $ ChatData $ "Welcome! Users: " `mappend` S.intercalate ", " users

    handleDisconnect name e = case fromException e of
        Just ConnectionClosed -> do
            liftIO $ modifyMVar_ clients $ return . M.delete name
            broadcast $ ChatData $ mconcat [name, " disconnected."]
        _ -> return ()

