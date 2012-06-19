{-# LANGUAGE GADTs #-}

module Network.Wai.Sock.Session
( SessionID
, SessionData(..)
, Session(..)
, newSessionData
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar (MVar, newMVar)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.ByteString      as SB (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
import qualified Data.Text            as ST (Text)
------------------------------------------------------------------------------
import           Network.Wai.Sock.Transport
------------------------------------------------------------------------------

data Session where
    Session :: Transport tag => TransportSession tag -> SessionData -> Session

type SessionID = ST.Text

-- | Right now, the function does not have to be wrapped in IO, but we will
-- have to add timers in the future here (and perhaps some MVars if needed).
newSessionData :: SessionID -> IO SessionData
newSessionData sid = return $ SessionData sid False False
    
    
data SessionData = SessionData
    { sessID          :: SessionID
    , sessInitialized :: Bool
    , sessClosed      :: Bool
    }
