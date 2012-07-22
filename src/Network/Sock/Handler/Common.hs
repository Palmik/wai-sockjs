{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Handler.Common
( respondSource
, respondLBS
, respondFrame

, responseOptions

, pollingSource
, streamingSource

, yieldAndFlush
, liftSTM
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad.Trans           (MonadIO, liftIO, lift)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString      as BS
import qualified Data.Conduit         as C  
import           Data.Proxy
import           Data.Int                   (Int64)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (Status)
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Frame
import           Network.Sock.Request
import           Network.Sock.Session
import           Network.Sock.Types.Protocol
import           Network.Sock.Types.Handler
------------------------------------------------------------------------------

-- | The default Source for polling transports.
-- It assumes that the SessionStatus MVar is empty (as it should be).
pollingSource :: Handler tag
              => Proxy tag           -- ^ Handler tag.
              -> Request             -- ^ Request we are responsing to.
              -> Session             -- ^ Associated session.
              -> Maybe SessionStatus -- ^ Status the session was in.
              -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
pollingSource tag req ses status =
    case status of
         Just (SessionFresh)  -> initialize >> yieldAndFlush (format' FrameOpen) >> setOpened
         Just (SessionOpened) -> liftSTM (getTMChanContents $ sessionOutgoingBuffer ses) >>= loop id
         Just (SessionClosed code reason) -> do
             yieldAndFlush (format' $ FrameClose code reason)
             setClosed
         Nothing -> yieldAndFlush $ format' $ FrameClose 2010 "Another connection still open"
    where loop front [] = yieldAndFlush (format' $ FrameMessages $ front []) >> setOpened
          loop front (x:xs) =
              case x of
                   Message s     -> loop (front . (s:)) xs
                   Raw s         -> yieldAndFlush s >> setOpened
                   Control Close -> do
                       -- yieldAndFlush (format' $ FrameMessages $ front []) -- ^ We mimic the way sockjs-node behaves.
                       yieldAndFlush (format' $ FrameClose 3000 "Go away!")
                       finalize >> setClosed
                       
          format' = format tag req
          setClosed = lift $ putMVar (sessionStatus ses) $ SessionClosed 3000 "Go away!"
          setOpened = lift $ putMVar (sessionStatus ses) SessionOpened
          initialize = lift $ initializeSession ses $ requestApplication req
          finalize = lift $ finalizeSession ses
{-# INLINE pollingSource #-}

-- | The default Source for streaming transports.
-- It assumes that the SessionStatus MVar is empty (as it should be).
streamingSource :: Handler tag
                => Proxy tag           -- ^ Handler tag.
                -> Request             -- ^ Request we are responsing to.
                -> Int64               -- ^ Maximum amount of bytes to be transfered (we can exceed the maximum if the last message is long, but the loop will stop).
                -> C.Source (C.ResourceT IO) (C.Flush B.Builder) -- ^ Prelude sent before any other data.
                -> Session             -- ^ Associated session.
                -> Maybe SessionStatus -- ^ Status the session was in.
                -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
streamingSource tag req limit prelude ses status =
    case status of
         Just (SessionFresh)  -> initialize >> prelude >> yieldOpenFrame >> loop 0
         Just (SessionOpened) -> prelude >> yieldOpenFrame >> loop 0
         Just (SessionClosed code reason) -> prelude >> yieldAndFlush (format' $ FrameClose code reason) >> setClosed
         Nothing -> prelude >> yieldAndFlush (format' $ FrameClose 2010 "Another connection still open")
    where loop n = liftSTM (readTMChan $ sessionOutgoingBuffer ses) >>=
              maybe (return ())
                    (\x -> do
                        case x of
                             Message s     -> do
                                 let load = format' (FrameMessages [s])
                                 let newn = n + BL.length load
                                 yieldAndFlush load
                                 if newn < limit
                                    then loop newn
                                    else setOpened
                             Raw s         -> do
                                 let load = s
                                 let newn = n + BL.length load
                                 yieldAndFlush load
                                 if newn < limit
                                    then loop newn
                                    else setOpened
                             Control Close -> do
                                 yieldAndFlush (format' $ FrameClose 3000 "Go away!")
                                 finalize >> setClosed
                    )
          format' = format tag req
          setClosed = lift $ putMVar (sessionStatus ses) $ SessionClosed 3000 "Go away!"
          setOpened = lift $ putMVar (sessionStatus ses) SessionOpened
          initialize = lift $ initializeSession ses $ requestApplication req
          finalize = lift $ finalizeSession ses
          yieldOpenFrame = yieldAndFlush $ format' FrameOpen
{-# INLINE streamingSource #-}

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically
{-# INLINE liftSTM #-}

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/<server_id>/<session_id>/<transport>
--
-- Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-7
-- TODO: Put somewhere else.
responseOptions :: (H.IsResponse res, H.IsRequest req)
                => [BS.ByteString]
                -> req
                -> res
responseOptions methods req = H.response204 headers ""
    where headers =    [("Access-Control-Allow-Methods", BS.intercalate ", " methods)]
                    ++ H.headerCached
                    ++ H.headerCORS "*" req
{-# INLINE responseOptions #-}

respondSource :: (H.IsResponse res, Handler tag)
              => Proxy tag
              -> Request
              -> H.Status
              -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
              -> res
respondSource tag req status source = H.responseSource status (headers tag req) source
{-# INLINE respondSource #-}

respondFrame :: (H.IsResponse res, Handler tag)
             => Proxy tag
             -> Request
             -> H.Status
             -> Frame
             -> res
respondFrame tag req st fr = respondLBS tag req st (format tag req fr)
{-# INLINE respondFrame #-}

respondLBS :: (H.IsResponse res, Handler tag)
           => Proxy tag
           -> Request
           -> H.Status
           -> BL.ByteString
           -> res
respondLBS tag req status body = H.responseLBS status (headers tag req) body
{-# INLINE respondLBS #-}

-- | Yields a Chunk (a ByteString) and then Flushes.
yieldAndFlush :: Monad m => BL.ByteString -> C.Pipe l i (C.Flush B.Builder) u m ()
yieldAndFlush load = C.yield (C.Chunk $ B.fromLazyByteString load) >> C.yield C.Flush
{-# INLINE yieldAndFlush #-}