{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Transport
( Transport(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BL
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Response as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Request
------------------------------------------------------------------------------

-- | Transport
class Transport tag where
    handleIncoming :: Proxy tag
                   -> Request
                   -> Server H.Response

    -- | Formats the Frame (different protocols may format frames differently).
    format :: Proxy tag
           -> Frame
           -> BL.ByteString

    -- | Used to create a response (headers might be transport & request dependent).
    respond :: Proxy tag
            -> H.Status
            -> BL.ByteString
            -> Request
            -> H.Response

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