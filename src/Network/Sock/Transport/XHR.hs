{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.XHR
( XHRPolling
, XHRStreaming
, XHRSend
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad
import           Control.Monad.Base                          (MonadBase, liftBase)
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans.Class                   (lift)
------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as C
import qualified Data.Conduit.TMChan        as C  (sourceTMChan)
import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy       as BL (ByteString, length)
import qualified Data.ByteString.Lazy.Char8 as BL (replicate)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status204, status200)
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
data XHRPolling = XHRPolling

-- | XHRPolling Transport represents the /xhr route.
--   The /xhr route serves only to open sessions and to request data from them.
instance Transport XHRPolling where
    handleIncoming tag req =
        case requestMethod req of
             "POST"    -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

        where
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respondFrame200 tag FrameOpen req)

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO ses = do
                -- TODO: Reset the timeout timer.
                (msgs, rest) <- span isMessage <$> atomically (getTMChanContents $ sessionOutgoingBuffer ses)
                when (null msgs || not (null rest))
                     (closeSession ses)
                case msgs of
                     [] -> return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req) -- This should not happen since it means that the channel is closed.
                     xs | not $ null rest -> return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)
                        | otherwise       -> return (SessionOpened, respondFrame200 tag (FrameMessages (map fromMessage xs)) req)

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: H.IsResponse res => Session -> Server res
            handleW _ = return $ respondFrame200 tag (FrameClose 2010 "Another connection still open") req

            sid = requestSessionID req
            app = requestApplication req

    format _ fr _ = encodeFrame fr <> "\n"

    respond _ f st str req = f st headers str
        where headers =    H.headerJS
                        <> H.headerCORS "*" req
                        <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data XHRStreaming = XHRStreaming

-- | XHRStreaming Transport represents the /xhr_streaming route.
--   The /xhr_streaming route serves only to open sessions and to receive stream of incoming messages.
instance Transport XHRStreaming where
    handleIncoming tag req =
        case requestMethod req of
             "POST"    -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

        where
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respond tag H.responseSource H.status200 source req)
                where source = do
                          C.yield $ C.Chunk $ B.fromLazyByteString $ BL.replicate 2048 'h' <> "\n"
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

    format _ fr _ = encodeFrame fr <> "\n"

    respond _ f st str req = f st headers str
        where headers =    H.headerJS
                        <> H.headerCORS "*" req
                        <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data XHRSend = XHRSend

-- | XHRPolling Transport represents the /xhr_send route.
--   The /xhr_send route serves only to send data to a session (Application).
instance Transport XHRSend where
    handleIncoming tag req =
        case requestMethod req of
             "POST"    -> do
                 ms <- lookupSession sid
                 case ms of
                      Nothing -> return H.response404 -- ^ Sending to non-existing session results in 404. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-79)
                      Just s  -> handleByStatus tag handleF handleO handleC handleW s
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

        where
            -- | It should never come to this handler, since XHRSend never creates a session.
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF = handleC

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO ses = (\x -> (SessionOpened, x)) <$> handleW ses

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: H.IsResponse res => Session -> Server res
            handleW ses = do
                (body, cont) <- lift $ (\x -> (x, decode x)) <$> requestBodyConsumed req
                case cont of
                     Just xs -> do
                         atomically $ writeTMChanList (sessionIncomingBuffer ses) xs
                         return $ respondLBS tag H.status204 "" req
                     Nothing | body == "" -> return $ respondLBS tag H.status500 "Payload expected." req     -- If the body of request is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                             | otherwise  -> return $ respondLBS tag H.status500 "Broken JSON encoding." req -- If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)

            decode :: BL.ByteString -> Maybe [BL.ByteString]
            decode = AE.decode

            sid = requestSessionID req
            app = requestApplication req

    format _ fr _ = encodeFrame fr <> "\n"

    respond _ f st str req = f st headers str
        where headers =   H.headerPlain
                       <> H.headerCORS "*" req
                       <> H.headerJSESSIONID req
                                                                             