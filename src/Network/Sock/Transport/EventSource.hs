{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.EventSource
( EventSource
) where

------------------------------------------------------------------------------
import           Control.Monad
import           Control.Monad.Base                          (MonadBase, liftBase)
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans.Class                   (lift)
------------------------------------------------------------------------------
import qualified Data.Conduit               as C
import qualified Data.Conduit.TMChan        as C  (sourceTMChan)
import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy       as BL (ByteString, length)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status200)
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Types.Transport
import           Network.Sock.Application
import           Network.Sock.Frame
import           Network.Sock.Protocol
import           Network.Sock.Session
import           Network.Sock.Server
import           Network.Sock.Request
import           Network.Sock.Transport.Utility
------------------------------------------------------------------------------

atomically :: MonadBase IO m => STM.STM a -> m a
atomically = liftBase . STM.atomically

------------------------------------------------------------------------------
-- |
data EventSource = EventSource

-- | EventSource Transport represents the /eventsource route.
--   The /eventsource route serves only to open sessions and to receive stream of incoming messages.
instance Transport EventSource where
    handleIncoming tag req =
        case requestMethod req of
             "GET"    -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

        where
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respond tag H.responseSource H.status200 source req)
                where source = do
                          C.yield $ C.Chunk $ B.fromLazyByteString $ "\r\n"
                          C.yield C.Flush
                          C.yield $ C.Chunk $ B.fromLazyByteString $ format tag FrameOpen req
                          C.yield C.Flush
                          streamSource ses

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO = handleF

            streamSource :: Session -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
            streamSource ses = C.sourceTMChan (sessionOutgoingBuffer ses) C.$= loop 0
                where loop n = C.await >>=
                          maybe (return ())
                                (\x ->
                                    case x of
                                         Message s     -> do
                                             let load = format tag (FrameMessages [s]) req
                                             let newn = n + BL.length load
                                             C.yield $ C.Chunk $ B.fromLazyByteString load
                                             C.yield C.Flush
                                             when (newn < limit) $ loop newn
                                         Raw s         -> do
                                             let load = s
                                             let newn = n + BL.length load
                                             C.yield $ C.Chunk $ B.fromLazyByteString load
                                             C.yield C.Flush
                                             when (newn < limit) $ loop newn
                                         Control Close -> do
                                             let load = format tag (FrameClose 3000 "Go away!") req
                                             C.yield $ C.Chunk $ B.fromLazyByteString load
                                             C.yield C.Flush
                                             closeSession ses
                                         _             -> closeSession ses
                                )
                      limit = 4096



            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: H.IsResponse res => Session -> Server res
            handleW _ = return $ respondFrame200 tag (FrameClose 2010 "Another connection still open") req

            sid = requestSessionID req
            app = requestApplication req

    format _ fr _ = "data: " <> encodeFrame fr <> "\r\n\r\n"

    respond _ f st str req = f st headers str
        where headers =    H.headerEventStream
                        <> H.headerNotCached
                        <> H.headerCORS "*" req
                        <> H.headerJSESSIONID req
                                                                                                     