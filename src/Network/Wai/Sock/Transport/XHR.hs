{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Transport.XHR
( XHRPolling
, XHRSend
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.MVar.Extra.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad.Base                   (liftBase)
import           Control.Monad.STM                    (atomically)
------------------------------------------------------------------------------
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  ((<>))
import           Data.ByteString.Extra        (convertBL2BS)
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.ByteString.Lazy   as BL (ByteString, fromChunks)
import qualified Data.Conduit           as C
import qualified Data.Conduit.List      as C
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Request(..), responseLBS)
import           Network.Wai.Extra
import qualified Network.HTTP.Types as H (status500, status204)
------------------------------------------------------------------------------
import           Network.Wai.Sock.Environment
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Session
import           Network.Wai.Sock.Transport
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- |
data XHRPolling = XHRPolling

-- | XHRPolling Transport represents the /xhr route.
-- The /xhr route serves only to open sessions and to request data from them.
instance Transport XHRPolling where
    handleIncoming tag env req =
        case W.requestMethod req of
             "POST" -> getSession sid tag env >>= handleByStatus tag handleF handleO handleC handleW
             _      -> return response404 -- ^ TODO: Handle OPTIONS
        
        where
            handleF _ = do
                -- TODO: Start the timers.
                -- TODO: Fork the application.
                return (SessionOpened, respondFrame200 tag FrameOpen req) 

            handleO ses = do
                -- TODO: Reset the timeout timer.
                -- If the outgoing buffer is empty, we should wait until it's not so that we can send some response.
                -- If the outgoing buffer is not empty, we should send all messages as JSON encoded array of strings.
                let ch = sessionOutgoingBuffer ses
                liftBase . atomically $ do
                    closed <- isClosedTMChan ch
                    empty  <- isEmptyTMChan ch
                    case () of
                         _ | closed    -> return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req) -- This should not happen (we close the channel only when we close the session)
                           | empty     -> (\x -> (SessionOpened, respondFrame200 tag (FrameMessages [convertBL2BS $ fromJust x]) req)) <$> readTMChan ch -- We could use TupleSections extension here instead.
                           | otherwise -> (\x -> (SessionOpened, respondFrame200 tag (FrameMessages (map convertBL2BS x)) req)) <$> getTMChanContents ch -- We could use TupleSections extension here instead.
                           
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)
            handleW _ = return $ respondFrame200 tag (FrameClose 2010 "Another connection still open") req
                       
            sid = reverse (W.pathInfo req) !! 1 -- TODO: Handle this properly, that is have some W.Request wrapper where (Maybe) SessionID, Application, Session, are stored.

    format _ str = encodeFrame str <> "\n"

    respond _ st str _ = W.responseLBS st [] str -- TODO: Add proper headers.

    receive _ ses = liftBase . atomically $ readTMChan $ sessionIncomingBuffer ses

    send _ ses = liftBase . atomically . writeTMChan (sessionOutgoingBuffer ses)

------------------------------------------------------------------------------
-- |
data XHRSend = XHRSend

-- | XHRPolling Transport represents the /xhr_send route.
-- The /xhr_send route serves only to send data to a session (Application).
instance Transport XHRSend where
    handleIncoming tag env req =
        case W.requestMethod req of
             "POST" -> do
                 mvms <- lookupSession sid env
                 case mvms of
                      Nothing  -> return response404 -- ^ Sending to non-existing session results in 404. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-79)
                      Just mvs -> getSession sid tag env >>= handleByStatus tag handleF handleO handleC handleW
             _      -> return response404 -- ^ TODO: Handle OPTIONS

        where
            handleF _ = return (SessionFresh, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleO ses = do
                -- TODO: Reset the timeout timer.
                -- If the body of request is empty, repost it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                -- If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                body <- BL.fromChunks <$> (W.requestBody req C.$$ C.consume)
                case () of
                     _ | body == ""           -> return $ (SessionOpened, respond tag H.status500 "Payload expected." req)
                       | not $ validJSON body -> return $ (SessionOpened, respond tag H.status500 "Broken JSON encoding." req)
                       | otherwise -> do
                           liftBase . atomically $ writeTMChan (sessionIncomingBuffer ses) body
                           return (SessionOpened, respond tag H.status204 "" req)

            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)
            handleW = (fmap snd) . handleO

            sid = reverse (W.pathInfo req) !! 1 -- TODO: Handle this properly, that is have some W.Request wrapper where (Maybe) SessionID, Application, Session, are stored.

    format _ str = encodeFrame str <> "\n"

    respond _ st str _ = W.responseLBS st [] str -- TODO: Add proper headers.

    receive _ ses = liftBase . atomically $ readTMChan $ sessionIncomingBuffer ses

    send _ ses = liftBase . atomically . writeTMChan (sessionOutgoingBuffer ses)
                                                                             