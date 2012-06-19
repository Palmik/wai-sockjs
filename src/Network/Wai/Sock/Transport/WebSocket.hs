{-# LANGUAGE TypeFamilies #-}

module Network.Wai.Sock.Transport.WebSocket
( WebSocket(..)
) where

------------------------------------------------------------------------------
import           Network.Wai.Sock.Transport
------------------------------------------------------------------------------

-- | The transport's tag.
data WebSocket = WebSocket

type instance TransportMonad WebSocket = IO

instance Transport WebSocket where
    