{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.JSONP
( JSONPPolling
, JSONPSend
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
import           Data.Monoid                      ((<>))
import           Data.ByteString.Extra
import qualified Data.ByteString.Lazy       as BL (ByteString, fromChunks, splitAt)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status200, urlDecode)
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
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
data JSONPPolling = JSONPPolling

-- | JSONPPolling Transport represents the /jsonp route.
--   The /jsonp route serves only to open sessions and to request data from them.
instance Transport JSONPPolling where
    handleIncoming tag req =
        case requestMethod req of
             "GET"     ->
                 case lookup "c" $ requestQuery req of
                      Just _  -> getSession sid >>= handleByStatus tag handleF handleO handleC handleW
                      Nothing -> return $ respondLBS tag req H.status500 "\"callback\" parameter required.\n"
             "OPTIONS" -> return $! responseOptions ["OPTIONS", "GET"] req
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

    format _ req fr =
        case lookup "c" $ requestQuery req of
             Just (Just c) -> convertBS2BL c <> "(" <> AE.encode (encodeFrame fr) <> ");\r\n"
             _             -> "\"callback\" parameter required.\n"

    headers _ req =   H.headerJS
                   <> H.headerNotCached
                   <> H.headerJSESSIONID req

------------------------------------------------------------------------------
-- |
data JSONPSend = JSONPSend

-- | JSONPPolling Transport represents the /jsonp_send route.
--   The /jsonp_send route serves only to send data to a session (Application).
instance Transport JSONPSend where
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
            -- | It should never come to this handler, since JSONPSend never creates a session.
            handleF :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleF = handleC

            handleO :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleO ses = (\x -> (SessionOpened, x)) <$> handleW ses

            handleC :: H.IsResponse res => Session -> Server (SessionStatus, res)
            handleC _ = return (SessionClosed, respondFrame200 tag req $ FrameClose 3000 "Go away!")

            handleW :: H.IsResponse res => Session -> Server res
            handleW ses = do
                body <- lift $
                    case lookup "Content-Type" $ requestHeaders req of
                         Just "application/x-www-form-urlencoded" -> do
                             (d, b) <- BL.splitAt 2 . BL.fromChunks <$> (requestBody req C.$= C.map (H.urlDecode True) C.$$ C.consume)
                             case d of
                                  "d=" -> return b
                                  _    -> return ""
                         _ -> requestBodyConsumed req
                case decode body of
                     Just xs -> do
                         atomically $ writeTMChanList (sessionIncomingBuffer ses) xs
                         return $ respondLBS tag req H.status200 "ok"
                     Nothing | body == "" -> return $ respondLBS tag req H.status500 "Payload expected.\n"     -- If the body of request is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                             | otherwise  -> return $ respondLBS tag req H.status500 "Broken JSON encoding.\n" -- If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)

            decode :: BL.ByteString -> Maybe [BL.ByteString]
            decode = AE.decode

            sid = requestSessionID req
            app = requestApplication req

    format _ _ fr = encodeFrame fr <> "\n"

    headers _ req =   H.headerPlain
                   <> H.headerCORS "*" req
                   <> H.headerJSESSIONID req
                                                                             