{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Types.Server
( Server(..)
, ServerSettings(..)
, ServerState(..)
, ServerEnvironment(..)
) where

------------------------------------------------------------------------------
import           Control.Monad.Trans.State.Strict (StateT)
import           Control.Concurrent.MVar.Lifted   (MVar)
------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM (HashMap)
import qualified Data.Text           as TS (Text)
import qualified Data.Conduit        as C  (ResourceT)
------------------------------------------------------------------------------
import           Network.Sock.Types.Application
import           Network.Sock.Types.Session
------------------------------------------------------------------------------

-- | Server monad.
--   TODO: Wrap in newtype.
type Server = StateT ServerState (C.ResourceT IO)

-- | Sever state.
data ServerState = ServerState
    { serverSettings :: ServerSettings
    , serverEnvironment :: ServerEnvironment
    , serverApplicationRouter :: [TS.Text] -> Maybe (Application (C.ResourceT IO))
    }

-- | Server settings.
data ServerSettings = ServerSettings
    { settingsSockVersion :: TS.Text
    }

-- | Server environment.
newtype ServerEnvironment = ServerEnvironment
    { environmentSessions :: MVar (HM.HashMap SessionID Session)
    }