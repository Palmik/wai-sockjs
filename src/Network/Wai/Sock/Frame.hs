module Network.Wai.Sock.Frame
( Frame(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString as BS (ByteString)
------------------------------------------------------------------------------

data Frame
    = FrameOpen
    | FrameHeartbeat
    | FrameMessages [BS.ByteString]
    | FrameClose Int BS.ByteString