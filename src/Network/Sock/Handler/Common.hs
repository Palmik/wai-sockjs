{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Handler.Common
where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad
import           Control.Monad.Trans                       (MonadIO, liftIO, lift)
import           Control.Monad.Trans.Resource.Extra  as R
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy  as BL
import qualified Data.ByteString       as BS
import qualified Data.Conduit          as C
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

-- | Ideally we would C.register putMvar and put different value there depending on context,
-- but currently registert does not support registering function of the form (a -> IO ()), but it should be
-- quite an easy fix.

-- | The default Source for polling transports.
pollingSource :: Handler tag
              => Proxy tag           -- ^ Handler tag.
              -> Request             -- ^ Request we are responsing to.
              -> Session             -- ^ Associated session.
              -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
pollingSource tag req ses = do
    status <- tryTakeMVar $! sessionStatus ses
    case status of
         Just (SessionFresh)  -> addCleanup ses $ \key ->
             initialize >> yieldAndFlush (format' FrameOpen) >> releaseOpened key
             
         Just (SessionOpened) -> addCleanup ses $ \key -> 
             liftSTM (getTMChanContents $ sessionOutgoingBuffer ses) >>= loop key id
             
         Just (SessionClosed code reason) -> addCleanup ses $ \key ->
             yieldAndFlush (format' $ FrameClose code reason) >> releaseClosed key
             
         Nothing -> yieldAndFlush $ format' $ FrameClose 2010 "Another connection still open"
    where        
        loop key front [] = yieldAndFlush (format' $ FrameMessages $ front []) >> releaseOpened key
        loop key front (x:xs) =
            case x of
                 Message s     -> loop key (front . (s:)) xs
                 Raw s         -> yieldAndFlush s >> releaseOpened key
                 Control Close -> do
                     -- yieldAndFlush (format' $ FrameMessages $ front []) -- ^ We mimic the way sockjs-node behaves.
                     yieldAndFlush (format' $ FrameClose 3000 "Go away!")
                     finalize >> releaseClosed key
                     
        format' = format tag req
        initialize = lift $! initializeSession ses $! requestApplication req
        finalize = lift $! finalizeSession ses
{-# INLINE pollingSource #-}

-- | The default Source for streaming transports.
streamingSource :: Handler tag
                => Proxy tag           -- ^ Handler tag.
                -> Request             -- ^ Request we are responsing to.
                -> Int64               -- ^ Maximum amount of bytes to be transfered (we can exceed the maximum if the last message is long, but the loop will stop).
                -> C.Source (C.ResourceT IO) (C.Flush B.Builder) -- ^ Prelude sent before any other data.
                -> Session             -- ^ Associated session.
                -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
streamingSource tag req limit prelude ses = do
    status <- tryTakeMVar $! sessionStatus ses
    case status of
         Just (SessionFresh)  -> addCleanup ses $ \key ->
             initialize >> prelude >> yieldOpenFrame >> loop key 0
             
         Just (SessionOpened) -> addCleanup ses $ \key ->
             prelude >> yieldOpenFrame >> loop key 0
             
         Just (SessionClosed code reason) -> addCleanup ses $ \key ->
             prelude >> yieldAndFlush (format' $ FrameClose code reason) >> releaseClosed key
             
         Nothing -> prelude >> yieldAndFlush (format' $ FrameClose 2010 "Another connection still open")
    where
        loop key n = liftSTM (readTMChan $ sessionOutgoingBuffer ses) >>=
            maybe (return ())
                  (\x -> do
                      case x of
                           Message s     -> do
                               let load = format' (FrameMessages [s])
                               let newn = n + BL.length load
                               yieldAndFlush load
                               if newn < limit
                                  then loop key newn
                                  else releaseOpened key
                           Raw s -> do
                               let load = s
                               let newn = n + BL.length load
                               yieldAndFlush load
                               if newn < limit
                                  then loop key newn
                                  else releaseOpened key
                           Control Close -> do
                               yieldAndFlush (format' $ FrameClose 3000 "Go away!")
                               finalize >> releaseClosed key
                  )
                    
        format' = format tag req
        initialize = lift $! initializeSession ses $! requestApplication req
        finalize = lift $! finalizeSession ses
        yieldOpenFrame = yieldAndFlush $ format' FrameOpen
{-# INLINE streamingSource #-}

------------------------------------------------------------------------------
-- | Common utility functions

addCleanup :: Session
           -> (ReleaseKeyF SessionStatus -> C.Source (C.ResourceT IO) (C.Flush B.Builder))
           -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
addCleanup ses fsrc = do
    key <- registerPutStatus ses
    C.addCleanup (flip unless $! R.releaseF key $! SessionClosed 1002 "Connection interrupted")
                 (fsrc key)

registerPutStatus :: Session -> C.Pipe l i o u (C.ResourceT IO) (ReleaseKeyF SessionStatus)
registerPutStatus ses =
    lift $! R.registerF (void . tryPutMVar (sessionStatus ses))
                        (SessionClosed 1002 "Connection interrupted")
{-# INLINE registerPutStatus #-}

releaseClosed :: ReleaseKeyF SessionStatus -> C.Pipe l i o u (C.ResourceT IO) ()
releaseClosed key = lift $! R.releaseF key $! SessionClosed 3000 "Go away!"
{-# INLINE releaseClosed #-}

releaseOpened :: ReleaseKeyF SessionStatus -> C.Pipe l i o u (C.ResourceT IO) () 
releaseOpened key = lift $! R.releaseF key $! SessionOpened
{-# INLINE releaseOpened #-}

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
