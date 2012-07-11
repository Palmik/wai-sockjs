{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Types.Transport
( Transport(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (Status, ResponseHeaders)
import qualified Network.HTTP.Types.Response as H (IsResponse(..))
import qualified Network.HTTP.Types.Request  as H (IsRequest(..))
------------------------------------------------------------------------------
import           Network.Sock.Types.Frame
import           Network.Sock.Types.Server
import           Network.Sock.Types.Request
------------------------------------------------------------------------------

-- | Transport
class Transport tag where
    handleIncoming :: H.IsResponse res
                   => Proxy tag
                   -> Request
                   -> Server res

    -- | Formats the Frame (different protocols may format frames differently).
    format :: H.IsRequest req
           => Proxy tag
           -> Frame
           -> req
           -> BL.ByteString

    -- | Used to create a response (headers might be transport & request dependent).
    respond :: H.IsResponse res
            => Proxy tag
            -> (H.Status -> H.ResponseHeaders -> a -> res)
            -> H.Status
            -> a
            -> Request
            -> res

    {-
    -- | Used for _ => 'Application' communication.
    -- Awaits a message from the Session's buffer (or empties the whole buffer if there are multiple messages in it).
    -- In case of WebSocket, we call receive (WS is the only transport why this function is neccessary).
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Source for the 'Application'.
    receive :: Proxy tag
            -> Session
            -> Server [BL.ByteString]

    -- | Used for 'Application' => _ communication
    -- The '_' could stand for e.g. some web app communication with out server Application
    -- This function is used to create the Sink for the 'Application'.
    send :: Proxy tag
         -> Session
         -> BL.ByteString
         -> Server ()
    -}