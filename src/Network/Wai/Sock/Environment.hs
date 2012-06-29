{-# LANGUAGE FlexibleContexts #-}

module Network.Wai.Sock.Environment
( Environment(..)
, insertSession
, lookupSession
, getSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
------------------------------------------------------------------------------
import qualified Data.HashMap.Strict  as HM (insert, lookup)
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Environment(..), Transport(..))
import           Network.Wai.Sock.Server
import           Network.Wai.Sock.Session
------------------------------------------------------------------------------


insertSession :: SessionID
              -> Session
              -> Server Session
insertSession sid s = do
    sessions <- envSessions <$> getServerEnvironment
    modifyMVar_ sessions (return . HM.insert sid s)
    return s

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



