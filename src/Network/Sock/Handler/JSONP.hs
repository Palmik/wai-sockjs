{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Handler.JSONP
( JSONPPolling
, JSONPSend
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Trans (lift)
------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import qualified Data.Conduit               as C
import qualified Data.Conduit.List          as C
import           Data.Monoid                      ((<>))
import qualified Data.ByteString.Extra      as BS (convertBS2BL)
import qualified Data.ByteString.Lazy       as BL (ByteString, fromChunks, splitAt)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status200, urlDecode)
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
data JSONPPolling = JSONPPolling

-- | JSONPPolling Handler represents the /jsonp route.
--   The /jsonp route serves only to open sessions and to request data from them.
instance Handler JSONPPolling where
    handleReuqest tag req =
        case requestMethod req of
             "GET"     ->
                 case lookup "c" $ requestQuery req of
                      Just (Just c) -> do
                          session <- getSession $ requestSessionID req
                          return $ respondSource tag req H.status200 $ pollingSource tag req session
                      Nothing       -> return $ respondLBS tag req H.status500 "\"callback\" parameter required.\n"
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
             _         -> return H.response404

    format _ req fr =
        case lookup "c" $ requestQuery req of
             Just (Just c) -> BS.convertBS2BL c <> "(" <> AE.encode (encodeFrame fr) <> ");\r\n"
             _             -> "\"callback\" parameter required.\n" -- ^ This should never happen, since we check for callback before we use this function. Maybe we should pass format and headers in record ADT instead of them being part of the typeclass?

    headers _ req =   H.headerJS
                   <> H.headerNotCached
                   <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data JSONPSend = JSONPSend

-- | JSONPPolling Handler represents the /jsonp_send route.
--   The /jsonp_send route serves only to send data to a session (Application).
instance Handler JSONPSend where
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
                     Nothing            -> processIncoming session

            processIncoming session = do
                (empty, decoded) <- lift $ (\x -> (x == "", decode x)) <$> requestBodyConsumedJSONP req
                case decoded of
                     Just msgs -> do
                         liftSTM $ writeTMChanList (sessionIncomingBuffer session) msgs                         
                         return $ respondLBS' H.status200 "ok" -- ^ Everything went OK, we respond with empty body and status 204.
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

requestBodyConsumedJSONP :: Request -> C.ResourceT IO BL.ByteString
requestBodyConsumedJSONP req =
    case lookup "Content-Type" $ requestHeaders req of
         Just "application/x-www-form-urlencoded" -> do
             (d, b) <- BL.splitAt 2 . BL.fromChunks <$> (requestBody req C.$= C.map (H.urlDecode True) C.$$ C.consume)
             case d of
                  "d=" -> return b
                  _    -> return ""
         _ -> requestBodyConsumed req
         