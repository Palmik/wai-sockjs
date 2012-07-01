{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Wai.Sock.Internal.Types
( Environment(..)

, Session(..)
, SessionID
, SessionStatus(..)

, Server(..)
, ServerState(..)
, ServerSettings(..)

, Transport(..)

, Request(..)

, Application(..)
, ApplicationSettings(..)
) where

------------------------------------------------------------------------------
import           Control.Concurrent.Lifted      (ThreadId)
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Trans.State.Lazy
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Conduit         as C  (Source, Sink, ResourceT)
import qualified Data.HashMap.Strict  as HM (HashMap)
import           Data.Proxy
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Request(..), Response(..))
import qualified Network.HTTP.Types as H (Status(..))
------------------------------------------------------------------------------
import           Network.Wai.Sock.Frame
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Request wrapper related types.

data Request = Request
    { requestRaw :: W.Request
    , requestSessionID :: SessionID
    , requestApplication :: Application (C.ResourceT IO)
    }

------------------------------------------------------------------------------
-- | Application related types.

data Application m = Application
    { applicationDefinition :: C.Source m BL.ByteString -> C.Sink BL.ByteString m () -> m ()
    , applicationSettings :: ApplicationSettings
    }

data ApplicationSettings = ApplicationSettings
    { settingsApplicationPrefix :: [TS.Text]
    , settingsCookiesNeeded :: Bool
    , settingsAllowedOrigins :: [TS.Text]
    , settingsWebsocketsEnabled :: Bool
    , settingsSockURL :: TS.Text
    }

------------------------------------------------------------------------------
-- | Transport related types.

-- | Transport
class Transport tag where
    handleIncoming :: Proxy tag
                   -> Request
                   -> Server W.Response

    -- | Formats the Frame (different protocols may format frames differently).
    format :: Proxy tag
           -> Frame
           -> BL.ByteString

    -- | Used to create a response (headers might be transport & request dependent).
    respond :: Proxy tag
            -> H.Status
            -> BL.ByteString
            -> Request
            -> W.Response

    -- | Used for _ => 'Application' communication.
    -- Awaits a message from the Session's buffer. In case of WebSocket, we call receive (WS is the only transport why this function is neccessary).
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Source for the 'Application'.
    receive :: Proxy tag
            -> Session
            -> Server (Maybe BL.ByteString)

    -- | Used for 'Application' => _ communication
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Sink for the 'Application'.
    send :: Proxy tag
         -> Session
         -> BL.ByteString
         -> Server ()
         
------------------------------------------------------------------------------
-- | Server related types.

type Server = StateT ServerState (C.ResourceT IO)

data ServerState = ServerState
    { serverSettings :: ServerSettings
    , serverEnvironment :: Environment
    , serverApplicationRouter :: [TS.Text] -> Maybe (Application (C.ResourceT IO))
    }

data ServerSettings = ServerSettings
    { settingsSockVersion :: TS.Text
    }

------------------------------------------------------------------------------
-- | Environment related types.

-- | Environment
newtype Environment = Environment
    { envSessions :: MVar (HM.HashMap SessionID Session)
    }

------------------------------------------------------------------------------
-- | Session related types.

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
