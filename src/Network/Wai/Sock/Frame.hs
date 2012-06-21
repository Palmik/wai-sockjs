module Network.Wai.Sock.Frame
( Frame(..)
) where

------------------------------------------------------------------------------
import qualified Data.Text as TS (Text, unpack)
------------------------------------------------------------------------------

data Frame
    = FrameOpen
    | FrameHeartbeat
    | FrameMessage TS.Text
    | FrameClose Int TS.Text