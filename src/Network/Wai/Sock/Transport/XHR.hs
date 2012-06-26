{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Transport.XHR
( XHRPolling
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.MVar.Extra.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Concurrent.STM.TMChan.Extra
import           Control.Monad.Base                   (liftBase)
import           Control.Monad.STM                    (atomically)
------------------------------------------------------------------------------
import           Data.Maybe (fromJust)
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Request(..))
import           Network.Wai.Extra
------------------------------------------------------------------------------
import           Network.Wai.Sock.Environment
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Session
import           Network.Wai.Sock.Transport
------------------------------------------------------------------------------

data XHRPolling = XHRPolling

-- | XHR Transport represents the /xhr route.
-- The /xhr route serves only to open sessions and to request data from them.
instance Transport XHRPolling where
    handleIncoming tag env req = case W.requestMethod req of
                                      "POST" -> do
                                           mvs <- getSession sid tag env
                                           mvar (return . frameResponse tag req $ FrameClose 2010 "Another connection still open") -- The MVar is empty, which means there is another connection still open.
                                                (\s -> case sessionStatus s of
                                                            SessionFresh   -> handleF s
                                                            SessionOpened  -> handleO s
                                                            SessionClosed  -> handleC s
                                                ) mvs
                                      _      -> return response404 -- ^ TODO: Handle OPTIONS
        where handleF s = do
                  -- TODO: Start the timers.
                  return ( s { sessionStatus = SessionOpened }
                         , frameResponse tag req FrameOpen
                         )

              handleO s = do
                  -- TODO: Reset the timeout timer.
                  -- If the outgoing buffer is empty, we should wait until it's not so that we can send some response.
                  -- If the outgoing buffer is not empty, we should send all messages as JSON encoded array of strings.
                  let ch = sessionOutgoingBuffer s
                  liftBase . atomically $ do
                      closed <- isClosedTMChan ch
                      empty  <- isEmptyTMChan ch
                      case () of
                           _ | closed    ->  -- ^ This should not happen (we close the channel only when we close the session)
                                   return ( s { sessionStatus = SessionClosed }
                                          , frameResponse tag req $ FrameClose 3000 "Go away!"
                                          )
                             | empty     -> (\x -> (s, frameResponse tag req $ FrameMessages [fromJust x])) <$> readTMChan ch -- ^ We could use TupleSections extension here instead.
                             | otherwise -> (\x -> (s, frameResponse tag req $ FrameMessages x)) <$> getTMChanContents ch -- ^ We could use TupleSections extension here instead.


              handleC s = do
                  return ( s
                         , frameResponse tag req $ FrameClose 3000 "Go away!"
                         )
              sid = "000"
                                         
                                       