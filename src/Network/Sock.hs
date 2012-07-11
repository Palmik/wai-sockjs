module Network.Sock
( message
, close

, mapMessage

, send
, receive
) where

------------------------------------------------------------------------------
import           Data.ByteString.Lazy as BL
import           Data.Conduit         as C
import           Data.Conduit.List    as C
------------------------------------------------------------------------------
import           Network.Sock.Protocol
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | 

message :: Monad m
        => BL.ByteString
        -> C.Pipe l i Protocol u m ()
message = C.yield . Message

close :: Monad m
      => C.Pipe l i Protocol u m ()
close = C.yield $ Control Close

------------------------------------------------------------------------------
-- |

mapMessage :: Monad m
           => C.Conduit BL.ByteString m Protocol
mapMessage = C.map Message

------------------------------------------------------------------------------
-- |
 
receive :: Monad m
        => C.Source m BL.ByteString
        -> m (Maybe BL.ByteString)
receive source = source C.$$ C.await

send :: Monad m
     => C.Sink Protocol m ()
     -> BL.ByteString
     -> m ()
send sink m = C.yield (Message m) C.$$ sink



 
