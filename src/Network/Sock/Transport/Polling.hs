module Network.Sock.Transport.Polling
(
) where

------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
import           Control.Monad
import           Control.Monad.Trans           (MonadIO, liftIO)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy    as BL (length)
import qualified Data.Conduit            as C
import qualified Data.Conduit.List.Extra as C
import           Data.Int                   (Int64)
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.Sock.Request
import           Network.Sock.Session
import           Network.Sock.Types.Frame
import           Network.Sock.Types.Protocol
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

pollingSource :: Transport tag
              => Proxy tag
              -> Session
              -> Request
              -> C.Source (C.ResourceT IO) (C.Flush B.Builder)
pollingSource ses req = undefined
              
              
{-# INLINE pollingSource #-}