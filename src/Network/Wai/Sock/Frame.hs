{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Network.Wai.Sock.Frame
( Frame(..)
) where
    
------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.Aeson                 as AE (encode, decode)
import qualified Data.ByteString.Lazy       as LB (ByteString, uncons)
import qualified Data.ByteString.Lazy.Char8 as LB (pack)
import           Data.Char
import           Data.Int                         (Int64)
import           Data.Monoid                      ((<>))
import qualified Data.Text                  as ST (Text, unpack)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H
import qualified Network.Wai        as W (Application, Request(..), Response(ResponseBuilder))
------------------------------------------------------------------------------

data Frame where
    OpenFrame      :: Frame
    HeartbeatFrame :: Frame
    MessageFrame   :: ST.Text -> Frame
    CloseFrame     :: Int64 -> ST.Text -> Frame

-- encodeFrame :: Frame -> LB.ByteString
-- encodeFrame (OpenFrame) = "o"
-- encodeFrame (HeartbeatFrame) = "h"
-- encodeFrame (MessageFrame x) = AE.encode x
-- encodeFrame (CloseFrame n m) = "c[" <> LB.pack (show n) <> ",\"" <> (LB.pack $ ST.unpack m) <> "\"]"
-- 
-- decodeFrame :: LB.ByteString -> Maybe Frame
-- decodeFrame s = case LB.uncons s of
--     Just (h, t) | h == toEnum (ord 'o') -> Just OpenFrame
--                 | h == toEnum (ord 'h') -> Just HeartbeatFrame
--                 | h == toEnum (ord 'a') -> MessageFrame <$> AE.decode t
--                 | h == toEnum (ord 'c') -> uncurry CloseFrame <$> AE.decode t
--     _                                   -> Nothing
    