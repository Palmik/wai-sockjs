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
import           Control.Concurrent.STM.TMChan
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
import qualified Network.Wai        as W (Request(..), Response(..))
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
    { applicationPrefix :: [TS.Text]
    }

------------------------------------------------------------------------------
-- | Transport related types.

-- | Transport
class Transport tag where
    handleIncoming :: MonadBaseControl IO m
                   => Proxy tag
                   -> Environment
                   -> W.Request
                   -> m W.Response

    -- | Formats the Frame (different protocols may format frames differently).
    format :: Proxy tag
           -> Frame
           -> BL.ByteString

    -- | Used for _ => 'Application' communication.
    -- Awaits a message from the Session's buffer. In case of WebSocket, we call receive (WS is the only transport why this function is neccessary).
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Source for the 'Application'.
    receive :: MonadBaseControl IO m
            => Proxy tag
            -> Session
            -> m BL.ByteString

    -- | Used for 'Application' => _ communication
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Sink for the 'Application'.
    send :: MonadBaseControl IO m
         => Proxy tag
         -> BL.ByteString
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
        , sessionIncomingBuffer :: TMChan BS.ByteString -- ^ This buffer is filled by calls to handleIncoming and later, we transform it into Source for the Application.
        , sessionOutgoingBuffer :: TMChan BS.ByteString -- ^ This buffer is filled by calls to send.
        } -> Session

-- | SessionID
type SessionID = TS.Text

-- | SessionStatus
data SessionStatus
       = SessionFresh   -- ^ Right after creation, Session is "Fresh"
       | SessionOpened  -- ^ Right after we send opening frame, Session is "Opened". We also start the timeout & heartbeat timer at this point.
       | SessionWaiting 
       | SessionClosed  -- ^ Right after we send closing frame, Session if "Closed".
