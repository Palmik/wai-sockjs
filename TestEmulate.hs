{-# LANGUAGE OverloadedStrings #-}
module TestEmulate where

import Debug.Trace
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Network.WebSockets
import Network.WebSockets.Emulate
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Blaze.ByteString.Builder (toByteString)
import Control.Concurrent.Chan
import Data.Enumerator
import Data.Attoparsec (parseOnly)
import Data.Aeson

data TestMessage = TestMessage String

instance fromJSON TestMessage where
    fromJSON (Array [v]) = TestMessage <$> fromJSON v

writeMsg :: StreamChan ByteString -> L.ByteString -> IO ()
writeMsg chan msg = do
    writeChan chan $ Chunks $ (:[]) $
        toByteString $ encodeFrame EmulateProtocol Nothing $ Frame True BinaryFrame msg

readMsg :: StreamChan ByteString -> IO (Maybe ByteString)
readMsg chan = do
    ms <- fmap toChunks $ readChan chan
    case ms of
        Nothing -> return Nothing
        Just s -> return $
            either (\err -> traceShow err Nothing)
                   (Just . (\xs -> S.concat $ ["a"]++xs++["\n"]) . L.toChunks . framePayload) $ parseOnly (decodeFrame EmulateProtocol)
                   s
  where toChunks EOF = Nothing
        toChunks (Chunks xs) = Just $ S.concat xs

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    sendTextData ("o"::Text)
    forever $ do
        msg <- receiveData
        sendTextData (msg::Text)

close :: TextProtocol p => Request -> WebSockets p ()
close req = do
    sendTextData ("o"::Text)
    sendTextData ("c[3000,\"Go away!\"]"::Text)

type ServerState p = Map Text (Sink p) 

chat :: TextProtocol p => Request -> WebSockets p ()
chat req = acceptRequest req
