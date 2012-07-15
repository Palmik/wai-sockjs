module Network.Sock.Protocol
( Protocol(..)
, ProtocolControl(..)

, isMessage
, isRaw
, fromMessage
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
------------------------------------------------------------------------------
import           Network.Sock.Types.Protocol
------------------------------------------------------------------------------

isMessage :: Protocol -> Bool
isMessage (Message _) = True
isMessage _           = False

isRaw :: Protocol -> Bool
isRaw (Raw _) = True
isRaw _       = False

fromMessage :: Protocol -> BL.ByteString
fromMessage (Message s) = s
fromMessage _           = error "Used fromMessage on non-message."

