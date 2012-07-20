{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Network.Sock.Handler.WebSocket
( runApplicationWS
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent                          (forkIO, killThread)
import           Control.Concurrent.MVar
import           Control.Concurrent.STM.TMChan.Extra
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
import           Network.Sock.Protocol
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
    atid <- liftIO . forkIO . C.runResourceT $ applicationDefinition
                (C.sourceTMChan sessionIncomingBuffer)
                (C.sinkTMChan sessionOutgoingBuffer)

    -- | Send the open frame.
    liftIO . WS.sendSink sink $ WS.textData $ format FrameOpen

    -- | This MVar serves as a baton to the forverSend a foreverReceive thread.
    baton <- liftIO $ newEmptyMVar     

    -- | Start sending data. 
    stid <- liftIO . forkIO . forever $ sendIteration sink baton

    -- | Start receiving data. Sadly we can not fork forverReceive, so we will have check whether the MVar is empty, if we could (if WebSockets p was an instance of MonadBaseControl IO), receiveIteration would be much cleaner.
    while $ receiveIteration baton

    -- | Wait until either foreverReceive or foreverSend puts in a value into baton.
    () <- liftIO $ takeMVar baton

    -- | Kill the threads.
    liftIO $ killThread atid
    liftIO $ killThread stid
    finalizeSession ses
    
    where -- | Send data until the buffer is closed.
          sendIteration sink baton =  do
              (msgs, rest) <- span isMessage <$> atomically (getTMChanContents $ sessionOutgoingBuffer)
              when (not $ null msgs) $ do
                   WS.sendSink sink $! WS.textData $! format $! FrameMessages $! map fromMessage msgs
              when (null msgs || not (null rest))
                   (tryPutMVar baton () >> return ()) -- We should finish asap.

          -- | Receive data until the connection is closed or something goes wrong, then close the session.
          receiveIteration baton = flip WS.catchWsError catchException $ do
              (body, cont) <- (\x -> (x, decode x)) <$> WS.receiveData
              case cont of
                   Just xs -> liftIO (atomically $ writeTMChanList sessionIncomingBuffer xs)
                   Nothing | body == "" -> return ()                   -- If the received string is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-69)
                           | otherwise  -> (liftIO $ tryPutMVar baton () >> return ()) -- If the received string is not valid JSON, close the connection. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-66)
              res <- liftIO $ isEmptyMVar baton
              liftIO $ print res
              return res
              where catchException = const $ return False -- Usually this will be the CloseException, but in any case, we should end.

while :: Monad m => m Bool -> m ()
while action = do
    res <- action
    if res
       then while action
       else return ()

format :: Frame -> BL.ByteString
format = encodeFrame

decode :: BL.ByteString -> Maybe [BL.ByteString]
decode = AE.decode

instance MonadBase IO (WS.WebSockets p) where
    liftBase = liftIO
