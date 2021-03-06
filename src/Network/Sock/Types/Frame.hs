module Network.Sock.Types.Frame
( Frame(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
------------------------------------------------------------------------------

data Frame
    = FrameOpen
    | FrameHeartbeat
    | FrameMessages [BL.ByteString]
    | FrameClose Int BL.ByteString