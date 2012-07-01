{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Network.Sock.Session
( Session(sessionApplicationThread, sessionID, sessionIncomingBuffer, sessionOutgoingBuffer, sessionStatus, sessionTransportTag)
, SessionStatus(..)
, SessionID
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.TMChan
------------------------------------------------------------------------------
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

-- | Session
data Session where
    Session :: Transport tag =>
        { sessionID :: SessionID
        , sessionTransportTag :: Proxy tag
        , sessionStatus :: MVar SessionStatus
        , sessionIncomingBuffer :: TMChan BL.ByteString -- ^ This buffer is filled by calls to handleIncoming and later, we transform it into Source for the Application.
        , sessionOutgoingBuffer :: TMChan BL.ByteString -- ^ This buffer is filled by calls to send.
        , sessionApplicationThread :: MVar (Maybe ThreadId) -- ^ If the MVar is empty, some thread is already trying to fork application.
                                                            --   If it contains Nothing, noone is forking nor has anyone forked yet.
                                                            --   If it contains Just a value, application was already forked.
        } -> Session

-- | SessionID
type SessionID = TS.Text

-- | SessionStatus
data SessionStatus
       = SessionFresh   -- ^ Right after creation, Session is "Fresh"
       | SessionOpened  -- ^ Right after we send opening frame, Session is "Opened". We also start the timeout & heartbeat timer at this point.
       | SessionWaiting
       | SessionClosed  -- ^ Right after we send closing frame, Session if "Closed".