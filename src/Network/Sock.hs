{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs             #-}

module Network.Sock
( renderClientFrame
, decodeClientFrame
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import           Prelude hiding             ((++))
import           Data.Char
import           Data.Monoid
import           Data.Aeson                 (encode, decode)
import qualified Data.Text.Lazy as T        (pack)
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy as LB (uncons)
import           Data.ByteString.Lazy.Char8 ({- Instances -})
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder            (Builder)
import           Blaze.ByteString.Builder.ByteString (fromLazyByteString)
import           Blaze.ByteString.Builder.Char.Utf8  (fromString, fromLazyText)
------------------------------------------------------------------------------
import           Network.Sock.Types
------------------------------------------------------------------------------

(++) :: Monoid a => a -> a -> a
(++) = mappend

renderClientFrame :: ClientFrame -> Builder
renderClientFrame (FrameOpen) = fromString "o"
renderClientFrame (FrameHeartbeat) = fromString "h"
renderClientFrame (FrameMessage x) = fromLazyByteString $ encode x
renderClientFrame (FrameClose n m) = fromLazyText $ "c[" ++ T.pack (show n) ++ "," ++ m ++ "\"]"

decodeClientFrame :: ByteString -> Maybe ClientFrame
decodeClientFrame s = case LB.uncons s of
    Just (h, t) | h == toEnum (ord 'o') -> Just FrameOpen
                | h == toEnum (ord 'h') -> Just FrameHeartbeat
                | h == toEnum (ord 'o') -> FrameMessage <$> decode t
                | h == toEnum (ord 'c') -> uncurry FrameClose <$> decode t
    _                                   -> Nothing


