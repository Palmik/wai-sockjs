module Network.Sock.Message
( Message(..)
, ControlMessage(..)

, isDataMessage
, isControlMessage
, isCloseSessionMessage

, fromDataMessage
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
------------------------------------------------------------------------------
import           Network.Sock.Types.Message
------------------------------------------------------------------------------

fromDataMessage :: Message -> BL.ByteString
fromDataMessage (DataMessage x) = x
fromDataMessage _               = error "fromDataMessage used on ControlMessage"

isControlMessage :: Message -> Bool
isControlMessage (ControlMessage _) = True
isControlMessage _                  = False

isDataMessage :: Message -> Bool
isDataMessage (DataMessage _) = True
isDataMessage _               = False

isCloseSessionMessage :: Message -> Bool
isCloseSessionMessage (ControlMessage CloseSession) = True
isCloseSessionMessage _                             = False