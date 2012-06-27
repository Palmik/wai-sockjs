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
    handleIncoming :: Proxy tag
                   -> Environment
                   -> W.Request
                   -> C.ResourceT IO W.Response

    -- | Formats the Frame (different protocols may format frames differently).
    format :: Proxy tag
           -> Frame
           -> BL.ByteString

    -- | Used to create a response (headers might be transport dependent).
    respond :: Proxy tag
            -> H.Status
            -> BL.ByteString
            -> W.Response

    -- | Used for _ => 'Application' communication.
    -- Awaits a message from the Session's buffer. In case of WebSocket, we call receive (WS is the only transport why this function is neccessary).
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Source for the 'Application'.
    receive :: Proxy tag
            -> Session
            -> C.ResourceT IO (Maybe BL.ByteString)

    -- | Used for 'Application' => _ communication
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Sink for the 'Application'.
    send :: Proxy tag
         -> Session
         -> BL.ByteString
         -> C.ResourceT IO ()

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
        } -> Session

-- | SessionID
type SessionID = TS.Text

-- | SessionStatus
data SessionStatus
       = SessionFresh   -- ^ Right after creation, Session is "Fresh"
       | SessionOpened  -- ^ Right after we send opening frame, Session is "Opened". We also start the timeout & heartbeat timer at this point.
       | SessionWaiting 
       | SessionClosed  -- ^ Right after we send closing frame, Session if "Closed".
