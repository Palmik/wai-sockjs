{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Sock.Session
( Session(..)
, SessionStatus(..)
, SessionID

, newSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Chan.Lifted
import           Control.Monad.Base
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------

newSession :: MonadBase IO m
           => SessionID
           -> m Session
newSession sid = Session sid SessionFresh <$> newChan

data Session = Session
    { sessionID :: SessionID
    , sessionStatus :: SessionStatus
    , sessionIncomingBuffer :: Chan BS.ByteString
    }

-- | SessionID
type SessionID = TS.Text

-- | SessionStatus
data SessionStatus
       = SessionFresh  -- ^ Right after creation, Session is "Fresh"
       | SessionOpened -- ^ Right after we send opening frame, Session is "Opened". We also start the timeout & heartbeat timer at this point.
       | SessionClosed -- ^ Right after we send closing frame, Session if "Closed".
