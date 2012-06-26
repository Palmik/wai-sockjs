module Network.Wai.Sock.Transport.WebSocket
( WebSocket
) where

------------------------------------------------------------------------------
import           Network.Wai.Sock.Transport
------------------------------------------------------------------------------

data WebSocket = WebSocket

instance Transport WebSocket where
    