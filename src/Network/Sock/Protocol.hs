module Network.Sock.Protocol
( Protocol(..)
, ProtocolControl(..)

, isMessage
, fromMessage
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy
------------------------------------------------------------------------------
import           Network.Sock.Types.Protocol
import           Network.Sock.Types.Transport
------------------------------------------------------------------------------

isMessage :: Protocol -> Bool
isMessage (Message _) = True
isMessage _           = False

fromMessage :: Protocol -> BL.ByteString
fromMessage (Message s) = s
fromMessage _           = error "Used fromMessage on non-message."

