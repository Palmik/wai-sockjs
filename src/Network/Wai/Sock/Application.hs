{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Application
( Application(..)
, ApplicationSettings(..)
, forkApplication
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Lifted      (fork)
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.Conduit.TMChan    as C (sourceTMChan, sinkTMChan)
import           Data.Default
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Application(..), ApplicationSettings(..), Session(..))
------------------------------------------------------------------------------

instance Default ApplicationSettings where
    def = ApplicationSettings
              { settingsApplicationPrefix = ["foo"]
              , settingsWebsocketsEnabled = True
              , settingsCookiesNeeded = True
              , settingsAllowedOrigins = ["*:*"]
              , settingsSockURL = "http://cdn.sockjs.org/sockjs-0.3.min.js"
              }

-- | If there is no forked Application associated with the given Session
--   and if noone is trying to fork one (the MVar is not empty), forks the
--   given Application and saves the `ThreadId` into the MVar.
forkApplication :: (MonadBaseControl IO m, MonadIO m)
                => Application m
                -> Session
                -> m Bool
forkApplication Application{..} Session{..} = 
    modifyMVar sessionApplicationThread $ \mt ->
        case mt of
             Nothing -> (\ti -> (Just ti, True)) <$> fork runApplication
             Just ti -> return (Just ti, False)
    where runApplication =
              applicationDefinition (C.sourceTMChan sessionIncomingBuffer)
                                    (C.sinkTMChan sessionOutgoingBuffer)

