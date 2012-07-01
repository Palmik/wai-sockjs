{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Wai.Sock.Transport.XHR
( XHRPolling
, XHRSend
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad.Base                          (MonadBase, liftBase)
import qualified Control.Monad.STM                    as STM (STM, atomically)
import           Control.Monad.Trans.Class                   (lift)
------------------------------------------------------------------------------
import qualified Data.Aeson             as AE
import           Data.Maybe                   (fromJust)
import           Data.Monoid                  ((<>))
import           Data.ByteString.Extra        (convertBL2BS)
import qualified Data.ByteString.Lazy   as BL (ByteString)
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Response(..), responseLBS)
import           Network.Wai.Extra
import qualified Network.HTTP.Types as H (status500, status204)
------------------------------------------------------------------------------
import           Network.Wai.Sock.Application
import           Network.Wai.Sock.Environment
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Session
import           Network.Wai.Sock.Server
import           Network.Wai.Sock.Request
import           Network.Wai.Sock.Transport
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
             "POST"    -> getSession sid tag >>= handleByStatus tag handleF handleO handleC handleW
             "OPTIONS" -> return . responseOptions ["OPTIONS", "POST"] $ requestRaw req
             _         -> return response404 -- ^ TODO: Handle OPTIONS
        
        where
            handleF :: Session -> Server (SessionStatus, W.Response)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respondFrame200 tag FrameOpen req) 

            handleO :: Session -> Server (SessionStatus, W.Response)
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
                           | empty     -> (\x -> (SessionOpened, respondFrame200 tag (FrameMessages [convertBL2BS $ fromJust x]) req)) <$> readTMChan ch
                           | otherwise -> (\x -> (SessionOpened, respondFrame200 tag (FrameMessages (map convertBL2BS x)) req)) <$> getTMChanContents ch

            handleC :: Session -> Server (SessionStatus, W.Response)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: Session -> Server W.Response
            handleW _ = return $ respondFrame200 tag (FrameClose 2010 "Another connection still open") req
                       
            sid = requestSessionID req
            app = requestApplication req

    format _ str = encodeFrame str <> "\n"

    respond _ st str req = W.responseLBS st headers str
        where headers =    headerJS
                        <> headerCORS "*" (requestRaw req)
                        <> headerJSESSIONID (requestRaw req)
          

    receive _ ses = atomically $ readTMChan $ sessionIncomingBuffer ses

    send _ ses = atomically . writeTMChan (sessionOutgoingBuffer ses)

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
                      Nothing -> return response404 -- ^ Sending to non-existing session results in 404. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-79)
                      Just s  -> handleByStatus tag handleF handleO handleC handleW s
             "OPTIONS" -> return . responseOptions ["OPTIONS", "POST"] $ requestRaw req
             _         -> return response404

        where
            -- | It should never come to this handler, since XHRSend never creates a session.
            handleF :: Session -> Server (SessionStatus, W.Response)
            handleF = handleC

            handleO :: Session -> Server (SessionStatus, W.Response)
            handleO ses = (\x -> (SessionOpened, x)) <$> handleW ses

            handleC :: Session -> Server (SessionStatus, W.Response)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: Session -> Server W.Response
            handleW ses = do
                (body, cont) <- (\x -> (x, decode x)) <$> lift (consumeRequestBody req)
                case cont of
                     Just xs -> do
                         atomically $ writeTMChanList (sessionIncomingBuffer ses) xs
                         return $ respond tag H.status204 "" req
                     Nothing | body == "" -> return $ respond tag H.status500 "Payload expected." req     -- If the body of request is empty, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)
                             | otherwise  -> return $ respond tag H.status500 "Broken JSON encoding." req -- If the body of request is not valid JSON, report it. (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-80)

            decode :: BL.ByteString -> Maybe [BL.ByteString]
            decode = AE.decode

            sid = requestSessionID req
            app = requestApplication req

    format _ str = encodeFrame str <> "\n"

    respond _ st str req = W.responseLBS st headers str
        where headers =    headerPlain
                        <> headerCORS "*" (requestRaw req)
                        <> headerJSESSIONID (requestRaw req)

    receive _ ses = atomically . readTMChan $ sessionIncomingBuffer ses

    send _ ses = atomically . writeTMChan (sessionOutgoingBuffer ses)
                                                                             