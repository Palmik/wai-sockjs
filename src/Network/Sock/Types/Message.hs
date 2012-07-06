module Network.Sock.Types.Message
( Message(..)
, ControlMessage(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
------------------------------------------------------------------------------

data Message = DataMessage BL.ByteString
             | ControlMessage ControlMessage

data ControlMessage = CloseSession