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
import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy       as BL (ByteString)
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
import           Network.Sock.Transport.Streaming
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
                return (SessionOpened, respondFrame200 tag req FrameOpen)

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO ses = do
                -- TODO: Reset the timeout timer.
                (msgs, rest) <- span isMessage <$> atomically (getTMChanContents $ sessionOutgoingBuffer ses)
                when (null msgs || not (null rest))
                     (closeSession ses)
                case msgs of
                     [] -> return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!") -- This should not happen since it means that the channel is closed.
                     xs | not $ null rest -> return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")
                        | otherwise       -> return (SessionOpened, respondFrame200 tag req $ FrameMessages (map fromMessage xs))

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")

            handleW :: H.IsResponse res => Session -> Server res
            handleW _ = return . respondFrame200 tag req $ FrameClose 2010 "Another connection still open"

            sid = requestSessionID req
            app = requestApplication req

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerJS
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
                return (SessionOpened, respondSource tag req H.status200 source)
                where source = do
                          C.yield $ C.Chunk $ B.fromLazyByteString $ BL.replicate 2048 'h' <> "\n"
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

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerJS
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
            handleC _ = return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")

            handleW :: H.IsResponse res => Session -> Server res
            handleW ses = do
                (body, cont) <- lift $ (\x -> (x, decode x)) <$> requestBodyConsumed req
                case cont of
                     Just xs -> do
                         atomically $ writeTMChanList (sessionIncomingBuffer ses) xs
                         return $ respondLBS tag req H.status204 ""
                     Nothing | body == "" -> return $ respondLBS tag req H.status500 "Payload expected."     -- If the body of request is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                             | otherwise  -> return $ respondLBS tag req H.status500 "Broken JSON encoding." -- If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)

            decode :: BL.ByteString -> Maybe [BL.ByteString]
            decode = AE.decode

            sid = requestSessionID req
            app = requestApplication req

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerPlain
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req
                                                                             