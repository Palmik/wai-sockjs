{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Network.Wai.Sock.Environment
( Environment(..)
, insertSession
, lookupSession
, adjustSession
, getSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
import qualified Data.HashMap.Strict  as HM (HashMap, insert, lookup)
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Environment(..), Transport(..))
import           Network.Wai.Sock.Session
------------------------------------------------------------------------------


insertSession :: MonadBaseControl IO m
              => SessionID
              -> Session
              -> Environment
              -> m (MVar Session)
insertSession sid s Environment{..} = do
    ms <- newMVar s
    modifyMVar_ envSessions (return . HM.insert sid ms)
    return ms

lookupSession :: MonadBaseControl IO m
              => SessionID
              -> Environment
              -> m (Maybe (MVar Session))
lookupSession sid Environment{..} = withMVar envSessions (return . HM.lookup sid)
    

-- | Applies the given function on session with given ID and saves the new value.
-- If session with the supplied ID does not exist, it's virtually no-op.
adjustSession :: MonadBaseControl IO m
              => (Session -> m Session)
              -> SessionID
              -> Environment
              -> m ()
adjustSession f sid Environment{..} = withMVar envSessions go
    where go smap = case HM.lookup sid smap of
                        Just ms -> modifyMVar_ ms f
                        Nothing -> return ()
                   

-- | Retrieves session with the given ID, if there is no such session, it's created first.
getSession :: (MonadBaseControl IO m, Transport tag)
           => SessionID
           -> Proxy tag
           -> Environment
           -> m (MVar Session)
getSession sid tr env = do
    mms <- lookupSession sid env
    case mms of
         Just ms -> return ms
         Nothing -> do
             s <- newSession sid tr
             insertSession sid s env
             
             
{-
getSession :: (MonadBaseControl IO m, Transport tag)
           => SessionID
           -> Proxy tag
           -> Environment
           -> m (MVar Session)
getSession sid tr Environment{..} = modifyMVar envSessions go
    where go smap = case HM.lookup sid smap of
                        Just ms -> return (smap, ms)
                        Nothing -> do
                            ms <- newSession sid tr >>= newMVar
                            return (HM.insert sid ms smap, ms)
-}



