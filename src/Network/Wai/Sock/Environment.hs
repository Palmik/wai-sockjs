{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Network.Wai.Sock.Environment
( Environment
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


addSession :: MonadBaseControl IO m
           => SessionID
           -> Session
           -> Environment
           -> m ()
addSession sid s Environment{..} = do
    ms <- newMVar s
    modifyMVar_ envSessions (return . HM.insert sid ms)

-- | Applies the given function on session with given ID and saves the new value.
-- If session with the supplied ID does not exist, it's virtually no-op.
modifySession :: MonadBaseControl IO m
              => (Session -> m Session)
              -> SessionID
              -> Environment
              -> m ()
modifySession f sid Environment{..} = withMVar envSessions go    
    where go smap = case HM.lookup sid smap of
                        Just ms -> modifyMVar_ ms f
                        Nothing -> return ()
                   

-- | Retrieves session with the given ID, if there is no such session, it's created first.
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



