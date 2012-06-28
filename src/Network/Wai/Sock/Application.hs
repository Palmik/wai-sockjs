{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Sock.Application
( Application(..)
, ApplicationSettings(..)
, runApplication
) where

------------------------------------------------------------------------------
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.Conduit           as C
import qualified Data.Conduit.TMChan    as C (sourceTMChan, sinkTMChan)
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Application(..), ApplicationSettings(..), Session(..))
------------------------------------------------------------------------------

runApplication :: (MonadBaseControl IO m, MonadIO m)
               => Application m
               -> Session
               -> m ()
runApplication Application{..} Session{..} =
    applicationDefinition (C.sourceTMChan sessionIncomingBuffer)
                          (C.sinkTMChan sessionOutgoingBuffer)

