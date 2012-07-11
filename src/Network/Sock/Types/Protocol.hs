module Network.Sock.Types.Protocol
( Protocol(..)
, ProtocolControl(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
------------------------------------------------------------------------------

-- | Data type representing the protocol used to encode outgoing messages.
data Protocol = Message BL.ByteString   -- ^ Interpreted as an element of a FrameMessages. Public.
              | Control ProtocolControl -- ^ Interpreted case by case.
              | Raw BL.ByteString       -- ^ Interpreted as is. Used to contain encoded FrameClose, FrameOpen, FrameHeartbeat and some other stuff.

-- | Data type to represend control messages.
data ProtocolControl = Close -- ^ Signifies that we wish to close the session. Enables the Application to close the session.
