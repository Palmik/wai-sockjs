{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Network.Sock.Session
( Session(..)
, SessionStatus(..)
, SessionID

, newSession

, insertSession
, lookupSession
, getSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Base
------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM (insert, lookup)
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Server
------------------------------------------------------------------------------

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
         Nothing -> do
             s <- newSession sid
             insertSession sid s