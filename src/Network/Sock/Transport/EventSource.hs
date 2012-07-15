{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.EventSource
( EventSource
) where

------------------------------------------------------------------------------
import           Control.Monad.Base                          (MonadBase, liftBase)
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans.Class                   (lift)
------------------------------------------------------------------------------
import qualified Data.Conduit as C
import           Data.Monoid        ((<>))
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
import           Network.Sock.Session
import           Network.Sock.Server
import           Network.Sock.Request
import           Network.Sock.Transport.Streaming
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
             "GET"     -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

        where
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respondSource tag req H.status200 source)
                where source = do
                          C.yield $ C.Chunk $ B.fromLazyByteString $ "\r\n"
                          C.yield C.Flush
                          C.yield $ C.Chunk $ B.fromLazyByteString $ format tag req FrameOpen
                          C.yield C.Flush
                          streamSource ses

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO = handleF

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")

            handleW :: H.IsResponse res => Session -> Server res
            handleW _ = return . respondFrame200 tag req $ FrameClose 2010 "Another connection still open"

            streamSource :: Session -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
            streamSource ses = streamingSource tag 4096 ses req

            sid = requestSessionID req
            app = requestApplication req

    format _ _ fr = "data: " <> encodeFrame fr <> "\r\n\r\n"

    headers _ req =   H.headerEventStream
                   <> H.headerNotCached
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req
                                                                                                     