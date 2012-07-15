{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Transport.Streaming
( streamingSource
) where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import           Control.Monad
import           Control.Monad.Trans           (MonadIO, liftIO)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (length)
import qualified Data.Conduit         as C
import           Data.Int                   (Int64)
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Request
import           Network.Sock.Session
import           Network.Sock.Types.Frame
import           Network.Sock.Types.Protocol
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

streamingSource :: Transport tag
                => Proxy tag
                -> Int64
                -> Session
                -> Request
                -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
streamingSource tag limit ses req = loop 0
    where loop n = liftSTM (readTMChan $ sessionOutgoingBuffer ses) >>=
              maybe (return ())
                    (\x ->
                        case x of
                             Message s     -> do
                                 let load = format tag req (FrameMessages [s])
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
                                 let load = format tag req (FrameClose 3000 "Go away!")
                                 C.yield $ C.Chunk $ B.fromLazyByteString load
                                 C.yield C.Flush
                                 closeSession ses
                             _             -> return ()
                    )
{-# INLINE streamingSource #-}

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
