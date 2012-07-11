{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Network.Sock.Types.Session
( Session(..)
, SessionStatus(..)
, SessionID
) where

------------------------------------------------------------------------------
import           Control.Concurrent.Lifted      (ThreadId)
import           Control.Concurrent.MVar.Lifted (MVar)
import           Control.Concurrent.STM.TMChan  (TMChan)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL     (ByteString)
import qualified Data.Text            as TS     (Text)
------------------------------------------------------------------------------
import           Network.Sock.Types.Protocol
------------------------------------------------------------------------------

-- | Session
data Session where
    Session ::
        { sessionID :: SessionID
        , sessionStatus :: MVar SessionStatus               -- ^ The status can be "fresh", "opened", "closed" or in case the MVar is empty, it should be interpreted as "currently being used"/"waiting"
        , sessionIncomingBuffer :: TMChan BL.ByteString     -- ^ This buffer is filled with incoming messages (parsed from request body or from WS' receive)
        , sessionOutgoingBuffer :: TMChan Protocol          -- ^ This buffer is filled with outgoing messages which are then sent (as a response or with WS' sendSink)
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
       | SessionClosed  -- ^ Right after we send closing frame, Session if "Closed".