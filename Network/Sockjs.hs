{-# LANGUAGE ExistentialQuantification, OverloadedStrings, DeriveDataTypeable #-}
module Network.Sockjs
  ( SockjsMessage (..)
  , renderSockjs
  , SockjsException (..)
  , SockjsRequest (..)
  , decodeSockjs
  , decodeValue
  ) where

import Debug.Trace
import Data.Maybe
import Prelude hiding ( (++) )
import Data.Typeable
import Data.Monoid
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Data.Aeson.Encode (fromValue)
import Data.Aeson.Parser (value)
import Data.Attoparsec
import Control.Exception
import Control.Applicative

(++) :: Monoid a => a -> a -> a
(++) = mappend

data SockjsRequest a = SockjsRequest { unSockjsRequest :: [a] }
    deriving (Show) -- for debug

instance FromJSON a => FromJSON (SockjsRequest a) where
    parseJSON js@(Array _) = SockjsRequest <$> parseJSON js
    parseJSON js = SockjsRequest . (:[]) <$> parseJSON js

data SockjsMessage = SockjsOpen
                   | SockjsHeartbeat
                   | SockjsData [ByteString]
                   | SockjsClose Int ByteString
    deriving (Show)

renderSockjs :: SockjsMessage -> Builder
renderSockjs msg = case msg of
    SockjsOpen -> B.fromByteString "o"
    SockjsHeartbeat -> B.fromByteString "h"
    (SockjsData xs) -> mconcat $
                    [ B.fromByteString "a"
                    , (fromValue . toJSON $ xs)
                    ]
    (SockjsClose code reason) -> B.fromLazyByteString . L.fromChunks $ 
                    [ "c["
                    , S.pack (show code)
                    , ",\""
                    , reason
                    , "\"]"
                    ]

decodeValue :: (FromJSON a) => ByteString -> Maybe a
decodeValue s = case parse value s of
             Done _ v -> case fromJSON v of
                             Success a -> Just a
                             _         -> Nothing
             _          -> Nothing

decodeSockjs :: ByteString -> Maybe SockjsMessage
decodeSockjs s = case S.uncons s of
    Just ('o', _) -> Just SockjsOpen 
    Just ('h', _) -> Just SockjsHeartbeat 
    Just ('a', s') -> SockjsData <$> decodeValue s'
    Just ('c', s') -> do (code, reason) <- decodeValue s'
                         return $ SockjsClose code reason
    _ -> trace ("unknown message:"++show s) Nothing

data SockjsException = SockjsReadEOF
                     | SockjsError String
    deriving (Show, Typeable)

instance Exception SockjsException

