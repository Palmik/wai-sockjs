{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Frame
( Frame(..)
, encodeFrame
, validJSON
) where

------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE
import qualified Data.ByteString            as BS (ByteString)
import qualified Data.ByteString.Lazy       as BL (ByteString, fromChunks, intercalate)
import qualified Data.ByteString.Lazy.Char8 as BL (pack)
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
encodeFrame (FrameMessages xs) = "a[" <> BL.intercalate "," (map wrap xs) <> "]"
encodeFrame (FrameClose n m) = "c[" <> BL.pack (show n) <> "," <> wrap m <> "]"

-- Converts strict ByteString to lazy ByteString and wraps it into quotation marks.
wrap x = "\"" <> BL.fromChunks [x] <> "\""

validJSON :: BL.ByteString -> Bool
validJSON str = maybe False (const True) $ ((AE.decode str) :: Maybe AE.Value)