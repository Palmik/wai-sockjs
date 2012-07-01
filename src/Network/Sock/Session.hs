{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Network.Sock.Session
( Session(sessionApplicationThread, sessionID, sessionIncomingBuffer, sessionOutgoingBuffer, sessionStatus, sessionTransportTag)
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
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Sock.Types.Session
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

-- | Clever constructor. Sessions should be created using this function only.
newSession :: (MonadBase IO m, Transport tag)
           => SessionID
           -> Proxy tag
           -> m Session
newSession sid tr = Session sid tr <$> newMVar SessionFresh
                                   <*> liftBase newTMChanIO
                                   <*> liftBase newTMChanIO
                                   <*> newMVar Nothing


-- | Inserts a new Session under the given SessionID.
insertSession :: SessionID
              -> Session
              -> Server Session
insertSession sid s = do
    sessions <- envSessions <$> getServerEnvironment
    modifyMVar_ sessions (return . HM.insert sid s)
    return s

-- | Looks up a Session with the given SessionID.
lookupSession :: SessionID
              -> Server (Maybe Session)
lookupSession sid = do
    sessions <- envSessions <$> getServerEnvironment
    withMVar sessions (return . HM.lookup sid)


-- | Retrieves session with the given ID, if there is no such session, it's created first.
getSession :: Transport tag
           => SessionID
           -> Proxy tag
           -> Server Session
getSession sid tr = do
    mms <- lookupSession sid
    case mms of
         Just ms -> return ms
         Nothing -> do
             s <- newSession sid tr
             insertSession sid s