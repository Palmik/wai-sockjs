{-# LANGUAGE GADTs            #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Sock.Internal.Types
( Environment(..)

, Session(..)
, SessionID
, SessionStatus(..)

, Transport(..)

, Application(..)
, ApplicationSettings(..)
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
import qualified Data.HashMap.Strict  as HM (HashMap)
import           Data.Proxy
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Request(..))
------------------------------------------------------------------------------
import           Network.Wai.Sock.Frame
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Application related types.

data Application m = Application
    { applicationDefinition :: C.Source m BL.ByteString -> C.Sink BL.ByteString m () -> m ()
    , applicationSettings :: ApplicationSettings
    }

data ApplicationSettings = ApplicationSettings
    { applicationSettingsPrefix :: [TS.Text]
    }

------------------------------------------------------------------------------
-- | Transport related types.

-- | Transport
class Transport tag where
    -- | Used for _ => Application communication.
    -- The request is checked whether it conforms the transport's rules and is then saved to a buffer.
    -- The '_' could stand for e.g. some web app communication with out server Application
    handleIncoming :: MonadBaseControl IO m
                   => Proxy tag
                   -> Environment
                   -> W.Request
                   -> m ()

    -- | Used for Application => _ communication
    -- The frame is checked whether it conforms the transport's rules and is then send (no buffering here).
    -- The '_' could stand for e.g. some web app communication with out server Application
    send :: MonadBaseControl IO m
         => Proxy tag
         -> Frame
         -> m ()

------------------------------------------------------------------------------
-- | Environment related types.

-- | Environment
newtype Environment = Environment
    { envSessions :: MVar (HM.HashMap SessionID (MVar Session))
    }

------------------------------------------------------------------------------
-- | Session related types.

-- | Session
data Session where
    Session :: Transport tag =>
        { sessionID :: SessionID
        , sessionTransportTag :: Proxy tag
        , sessionStatus :: SessionStatus
        , sessionIncomingBuffer :: Chan BS.ByteString
        } -> Session

-- | SessionID
type SessionID = TS.Text

-- | SessionStatus
data SessionStatus
       = SessionFresh  -- ^ Right after creation, Session is "Fresh"
       | SessionOpened -- ^ Right after we send opening frame, Session is "Opened". We also start the timeout & heartbeat timer at this point.
       | SessionClosed -- ^ Right after we send closing frame, Session if "Closed".
