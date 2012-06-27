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
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
import           Data.Proxy
import qualified Data.Text            as TS (Text)
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