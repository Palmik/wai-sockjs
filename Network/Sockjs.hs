{-# LANGUAGE OverloadedStrings #-}
module Network.Sockjs
  ( sendSockjs
  , sendSockjsData
  , sockjsData
  , sendSinkSockjs
  , receiveSockjs
  , startHBThread
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Monoid
import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Network.WebSockets
import Network.Sockjs.Types

heartBeatInterval :: Int
heartBeatInterval = 25

sendSockjs :: TextProtocol p => SockjsMessage -> WebSockets p ()
sendSockjs = sendTextData . B.toLazyByteString . renderSockjs

sendSockjsData :: TextProtocol p => ByteString -> WebSockets p ()
sendSockjsData = sendSockjs . SockjsData . (:[])

sockjsData :: (TextProtocol p, WebSocketsData a) => a -> Message p
sockjsData = textData . B.toLazyByteString . renderSockjs . SockjsData . (:[]) . mconcat . L.toChunks . toLazyByteString

sendSinkSockjs :: TextProtocol p => Sink p -> SockjsMessage -> IO ()
sendSinkSockjs sink = sendSink sink . textData . B.toLazyByteString . renderSockjs

receiveSockjs :: (TextProtocol p, FromJSON a, Monoid a) => WebSockets p a
receiveSockjs = mconcat <$> receiveSockjs'

receiveSockjs' :: (TextProtocol p, FromJSON a) => WebSockets p [a]
receiveSockjs' = do
    msg <- receiveData
    case msg of
        "" -> receiveSockjs'
        _ -> maybe (throwWsError (SockjsError "Broken JSON encoding [receive]."))
                   return
                   (unSockjsRequest <$> decodeValue msg)

startHBThread :: TextProtocol p => WebSockets p ThreadId
startHBThread = do
    sink <- getSink
    liftIO . forkIO . forever $ do
        threadDelay (heartBeatInterval*1000*1000)
        sendSinkSockjs sink SockjsHeartbeat
