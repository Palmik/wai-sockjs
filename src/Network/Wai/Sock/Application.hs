module Network.Wai.Sock.Application
( Application
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.ByteString      as SB (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)
------------------------------------------------------------------------------

type Application m = C.Source m LB.ByteString -> C.Sink LB.ByteString m () -> m ()
