{-# LANGUAGE OverloadedStrings #-}
module TestEmulate where

import Debug.Trace
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
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
import qualified Data.Vector as V
import qualified Data.Attoparsec.Enumerator as AE

data TestMessage = TestMessage String

instance FromJSON TestMessage where
    parseJSON (Array xs)
        | [x] <- V.toList xs = TestMessage <$> parseJSON x
    parseJSON _ = fail "parse fail"

instance ToJSON TestMessage where
    toJSON (TestMessage msg) = toJSON [toJSON msg]

writeMsg :: StreamChan ByteString -> L.ByteString -> IO ()
writeMsg chan = writeChan chan . Chunks . (:[]) .
                    toByteString . encodeFrame EmulateProtocol Nothing . Frame True BinaryFrame

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

receiveJSON :: (TextProtocol p, FromJSON a) => WebSockets p a
receiveJSON = do
    msg <- receiveData
    case decode msg of
        Nothing -> throwWsError $ ParseError $ AE.ParseError [] ""
        Just d -> return d

sendJSON :: (TextProtocol p, ToJSON a) => a -> WebSockets p ()
sendJSON x = sendTextData (encode x)

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    sendTextData ("o"::Text)
    forever $ do
        (TestMessage msg) <- receiveJSON
        sendJSON (TestMessage msg)

close :: TextProtocol p => Request -> WebSockets p ()
close req = do
    sendTextData ("o"::Text)
    sendTextData ("c[3000,\"Go away!\"]"::Text)

type ServerState p = Map Text (Sink p) 

chat :: TextProtocol p => Request -> WebSockets p ()
chat req = acceptRequest req
