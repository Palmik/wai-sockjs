{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}

module Network.Wai.Sock.Session
( Session(..)
, SessionStatus(..)
, SessionID

, newSession
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.STM.TMChan
import           Control.Monad.Base
------------------------------------------------------------------------------
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Session(..), SessionID(..), SessionStatus(..), Transport(..))
------------------------------------------------------------------------------

newSession :: (MonadBase IO m, Transport tag)
           => SessionID
           -> Proxy tag
           -> m Session
newSession sid tr = Session sid tr <$> newMVar SessionFresh
                                   <*> liftBase newTMChanIO
                                   <*> liftBase newTMChanIO
                                   <*> newMVar Nothing