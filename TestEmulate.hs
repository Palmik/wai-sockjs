module TestEmulate where

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

writeMsg :: StreamChan ByteString -> L.ByteString -> IO ()
writeMsg chan msg = do
    writeChan chan $ Chunks $ (:[]) $
        toByteString $ encodeFrame EmulateProtocol Nothing $ Frame True BinaryFrame msg

readMsg :: StreamChan ByteString -> IO (Maybe ByteString)
readMsg chan = do
    ms <- fmap toChunks $ readChan chan
    case ms of
        Nothing -> return Nothing
        Just s ->
            either (error . show) (return . Just . S.concat . L.toChunks . framePayload) $ parseOnly (decodeFrame EmulateProtocol) s
  where toChunks EOF = Nothing
        toChunks (Chunks xs) = Just $ S.concat xs

echo :: TextProtocol p => Request -> WebSockets p ()
echo req = do
    acceptRequest req
    forever $ do
        msg <- receiveData
        sendTextData (msg::Text)

close :: Protocol p => Request -> WebSockets p ()
close req = rejectRequest req "close"

type ServerState p = Map Text (Sink p) 

chat :: TextProtocol p => Request -> WebSockets p ()
chat req = acceptRequest req
