{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Handler.XHR
( XHRPolling
, XHRStreaming
, XHRSend
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans (lift)
------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Lazy       as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL (replicate)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status204, status200)
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Handler
import           Network.Sock.Frame
import           Network.Sock.Session
import           Network.Sock.Request
import           Network.Sock.Handler.Common
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data XHRPolling = XHRPolling

-- | XHRPolling Handler represents the /xhr route.
--   The /xhr route serves only to open sessions and to request data from them.
instance Handler XHRPolling where
    handleReuqest tag req =
        case requestMethod req of
             "POST"    -> do
                 session <- getSession $ requestSessionID req
                 return $ respondSource tag req H.status200 $ pollingSource tag req session
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerJS
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data XHRStreaming = XHRStreaming

-- | XHRStreaming Handler represents the /xhr_streaming route.
--   The /xhr_streaming route serves only to open sessions and to receive stream of incoming messages.
instance Handler XHRStreaming where
    handleReuqest tag req =
        case requestMethod req of
             "POST"    -> do
                 let prelude = yieldAndFlush $ BL.replicate 2048 'h' <> "\n"
                 session <- getSession $ requestSessionID req
                 return $ respondSource tag req H.status200 $ streamingSource tag req 4096 prelude session
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerJS
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data XHRSend = XHRSend

-- | XHRPolling Handler represents the /xhr_send route.
--   The /xhr_send route serves only to send data to a session (Application).
instance Handler XHRSend where
    handleReuqest tag req =
        case requestMethod req of
             "POST"    -> do
                 ms <- lookupSession $ requestSessionID req
                 case ms of
                      Nothing -> return H.response404 -- ^ Sending to non-existing session results in 404. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-79)
                      Just s  -> handle s
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "POST"] req
             _         -> return H.response404

        where
            handle session = do
                status <- tryTakeMVar $ sessionStatus session
                case status of
                     Just (SessionFresh)  -> return H.response404 -- ^ This should never happen, since we do not create sessions in this handler.
                     Just (SessionOpened) -> do
                         res <- processIncoming session
                         putMVar (sessionStatus session) SessionOpened
                         return res
                     Just (SessionClosed code reason) -> return $ respondFrame' H.status200 $ FrameClose code reason
                     Nothing -> processIncoming session

            processIncoming session = do
                (empty, decoded) <- lift $ (\x -> (x == "", decode x)) <$> requestBodyConsumed req
                case decoded of
                     Just msgs -> do
                         liftSTM $ writeTMChanList (sessionIncomingBuffer session) msgs
                         return $ respondLBS' H.status204 "" -- ^ Everything went OK, we respond with empty body and status 204.
                     Nothing | empty     -> return $ respondLBS' H.status500 "Payload expected.\n"     -- ^ If the body of request is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                             | otherwise -> return $ respondLBS' H.status500 "Broken JSON encoding.\n" -- ^ If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)

            decode :: BL.ByteString -> Maybe [BL.ByteString]
            decode = AE.decode
                       
            respondLBS'   = respondLBS tag req
            respondFrame' = respondFrame tag req             

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerPlain
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req
                                                                             