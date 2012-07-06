{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Sock.Transport.WebSocket
( runApplicationWS
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent                          (forkIO, killThread)
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Exception                           (fromException)
import           Control.Monad
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans                         (MonadIO, liftIO)
import           Control.Monad.Base
------------------------------------------------------------------------------
import qualified Data.Aeson           as AE
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Conduit         as C
import qualified Data.Conduit.TMChan  as C  (sourceTMChan, sinkTMChan)
------------------------------------------------------------------------------
import qualified Network.WebSockets          as WS
------------------------------------------------------------------------------
import           Network.Sock.Application
import           Network.Sock.Frame
import           Network.Sock.Message
import           Network.Sock.Session
------------------------------------------------------------------------------

atomically :: MonadIO m => STM.STM a -> m a
atomically = liftIO . STM.atomically

runApplicationWS :: Application (C.ResourceT IO)
                 -> Session
                 -> WS.Request
                 -> WS.WebSockets WS.Hybi00 ()
runApplicationWS app@Application{..} ses@Session{..} req = do
    -- | Some preliminary preparations.
    WS.acceptRequest req
    sink <- WS.getSink       

    -- | Start the application in background.
    atid <- liftIO $ forkIO $ C.runResourceT $ applicationDefinition
                (C.sourceTMChan sessionIncomingBuffer)
                (C.sinkTMChan sessionOutgoingBuffer)

    -- | Send the open frame.
    liftIO . WS.sendSink sink $ WS.textData $ format FrameOpen

    -- | Start receving and sending data.
    stid <- liftIO $ forkIO $ foreverSend sink
    foreverReceive

    -- | Kill the threads.
    liftIO $ killThread atid
    liftIO $ killThread stid
    
    where -- | Send data until the buffer is closed.
          foreverSend sink =  do
              (msgs, rest) <- span isDataMessage <$> atomically (getTMChanContents $ sessionOutgoingBuffer)
              let continue = not (null msgs || not (null rest))
              when (null msgs || not (null rest))
                   (closeSession ses)
              when (not $ null msgs) $ do
                   WS.sendSink sink $! WS.textData $! format $! FrameMessages $! map fromDataMessage msgs
              when continue $ foreverSend sink

          -- | Receive data until the connection is closed or something goes wrong, then close the session.
          foreverReceive = flip WS.catchWsError catchDisconnect $ do
              (body, cont) <- (\x -> (x, decode x)) <$> WS.receiveData
              case cont of
                   Just xs -> liftIO (atomically $ writeTMChanList sessionIncomingBuffer xs) >> foreverReceive
                   Nothing | body == "" -> foreverReceive   -- If the received string is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-69)
                           | otherwise  -> closeSession ses -- If the received string is not valid JSON, close the connection. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-66)
              where catchDisconnect e =
                        case fromException e of
                             Just WS.ConnectionClosed -> closeSession ses
                             _                        -> return ()

format :: Frame -> BL.ByteString
format = encodeFrame

decode :: BL.ByteString -> Maybe [BL.ByteString]
decode = AE.decode

instance MonadBase IO (WS.WebSockets p) where
    liftBase = liftIO
