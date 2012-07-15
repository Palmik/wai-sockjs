{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Types.Transport
( Transport(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import           Data.Proxy
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H (ResponseHeaders)
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
           -> req
           -> Frame
           -> BL.ByteString

    -- | Used to create a response (headers might be transport & request dependent).
    headers :: Proxy tag
            -> Request
            -> H.ResponseHeaders