{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Frame
( Frame(..)
, encodeFrame
) where

------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import qualified Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Lazy       as BL (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BL ({- INSTANCES -})
import           Data.Monoid                ((<>))
------------------------------------------------------------------------------

data Frame
    = FrameOpen
    | FrameHeartbeat
    | FrameMessages [BS.ByteString]
    | FrameClose Int BS.ByteString

encodeFrame :: Frame -> BL.ByteString
encodeFrame (FrameOpen) = "o"
encodeFrame (FrameHeartbeat) = "h"
encodeFrame (FrameMessages xs) = "a" <> AE.encode xs
encodeFrame (FrameClose n m) = "c[" <> AE.encode n <> "," <> AE.encode m <> "]"

-- TODO: Add special escaping as per http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-130