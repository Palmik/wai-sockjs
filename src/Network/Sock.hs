module Network.Sock
( send
, receive
) where

------------------------------------------------------------------------------
import           Data.ByteString.Lazy as BL
import           Data.Conduit         as C
------------------------------------------------------------------------------
import           Network.Sock.Message
------------------------------------------------------------------------------

receive :: Monad m
        => C.Source m BL.ByteString
        -> m (Maybe BL.ByteString)
receive source = source C.$$ C.await

send :: Monad m
     => C.Sink Message m ()
     -> Message
     -> m ()
send sink m = C.yield m C.$$ sink



 
