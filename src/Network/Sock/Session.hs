{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RecordWildCards  #-}

module Network.Sock.Session
( Session(..)
, SessionStatus(..)
, SessionID

, newSession
, initializeSession
, finalizeSession

, insertSession
, lookupSession
, getSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.Lifted            (killThread)
import           Control.Concurrent.MVar.Lifted
import qualified Control.Concurrent.MVar        as MV
import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TMChan
import           Control.Monad.IO.Class
import           Control.Monad.Base
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM (insert, lookup)
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Server
import           Network.Sock.Application
------------------------------------------------------------------------------

initializeSession :: (MonadBaseControl IO m, MonadIO m)
                  => Session
                  -> Application m
                  -> m ()
initializeSession ses app = do
    -- TODO: Start the timers.
    -- Start the application thread
    forkApplication ses app
    return ()

finalizeSession :: MonadBase IO m
                => Session
                -> m ()
finalizeSession ses@Session{..} = do
    -- TODO: Stop the timers.
    -- Stop the application thread.
    liftBase $ MV.withMVar sessionApplicationThread (maybe (return ()) killThread)

    -- Close the buffers.
    liftBase . atomically $ 
        closeTMChan sessionIncomingBuffer >>
        closeTMChan sessionOutgoingBuffer

-- | Clever constructor. Sessions should be created using this function only.
newSession :: MonadBase IO m
           => SessionID
           -> m Session
newSession sid = Session sid <$> newMVar SessionFresh
                             <*> liftBase newTMChanIO
                             <*> liftBase newTMChanIO
                             <*> newMVar Nothing

-- | Inserts a new Session under the given SessionID.
insertSession :: SessionID
              -> Session
              -> Server Session
insertSession sid s = do
    sessions <- environmentSessions <$> getServerEnvironment
    modifyMVar_ sessions (return . HM.insert sid s)
    return s

-- | Looks up a Session with the given SessionID.
lookupSession :: SessionID
              -> Server (Maybe Session)
lookupSession sid = do
    sessions <- environmentSessions <$> getServerEnvironment
    withMVar sessions (return . HM.lookup sid)


-- | Retrieves session with the given ID, if there is no such session, it's created first.
getSession :: SessionID
           -> Server Session
getSession sid = do
    mms <- lookupSession sid
    case mms of
         Just ms -> return ms
         Nothing -> newSession sid >>= insertSession sid
         