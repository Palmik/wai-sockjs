{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Transport.XHR
( XHRPolling
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
import qualified Data.Aeson              as AE
import           Data.Monoid                   ((<>))
import qualified Data.ByteString.Lazy    as BL (ByteString)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (status500, status204)
import qualified Network.HTTP.Types.Response as H
import qualified Network.HTTP.Types.Extra    as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Transport
import           Network.Sock.Application
import           Network.Sock.Frame
import           Network.Sock.Message
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
             "OPTIONS" -> return . responseOptions ["OPTIONS", "POST"] $ requestRaw req
             _         -> return H.response404
        
        where
            handleF :: Session -> Server (SessionStatus, H.Response)
            handleF ses = do
                -- TODO: Start the timers.
                lift $ forkApplication app ses
                return (SessionOpened, respondFrame200 tag FrameOpen req) 

            handleO :: Session -> Server (SessionStatus, H.Response)
            handleO ses = do
                -- TODO: Reset the timeout timer.
                (msgs, rest) <- span isDataMessage <$> atomically (getTMChanContents $ sessionOutgoingBuffer ses)                     
                when (null msgs || not (null rest))
                     (closeSession ses)
                case msgs of
                     [] -> return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req) -- This should not happen since it means that the channel is closed.
                     xs | not $ null rest -> return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)
                        | otherwise       -> return (SessionOpened, respondFrame200 tag (FrameMessages (map fromDataMessage xs)) req)

            handleC :: Session -> Server (SessionStatus, H.Response)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: Session -> Server H.Response
            handleW _ = return $ respondFrame200 tag (FrameClose 2010 "Another connection still open") req
                       
            sid = requestSessionID req
            app = requestApplication req

    format _ str = encodeFrame str <> "\n"

    respond _ st str req = H.response st headers str
        where headers =    H.headerJS
                        <> H.headerCORS "*" (requestRaw req)
                        <> H.headerJSESSIONID (requestRaw req)

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
             "OPTIONS" -> return . responseOptions ["OPTIONS", "POST"] $ requestRaw req
             _         -> return H.response404

        where
            -- | It should never come to this handler, since XHRSend never creates a session.
            handleF :: Session -> Server (SessionStatus, H.Response)
            handleF = handleC

            handleO :: Session -> Server (SessionStatus, H.Response)
            handleO ses = (\x -> (SessionOpened, x)) <$> handleW ses

            handleC :: Session -> Server (SessionStatus, H.Response)
            handleC _ = return (SessionClosed, respondFrame200 tag (FrameClose 3000 "Go away!") req)

            handleW :: Session -> Server H.Response
            handleW ses = do
                let (body, cont) = (requestBody req, decode $ requestBody req)
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

    respond _ st str req = H.response st headers str
        where headers =    H.headerPlain
                        <> H.headerCORS "*" (requestRaw req)
                        <> H.headerJSESSIONID (requestRaw req)
                                                                             