{-# LANGUAGE ExistentialQuantification, OverloadedStrings, DeriveDataTypeable #-}
module Types
  ( SockjsMessage (..)
  , renderSockjs
  , SockjsException (..)
  , SockjsRequest (..)
  , decodeSockjs
  ) where

import Debug.Trace
import Prelude hiding ( (++) )
import Data.Typeable
import Data.Monoid
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson
import Data.Aeson.Encode (fromValue)
import Data.Aeson.Parser (value)
import qualified Data.Attoparsec.Lazy as L
import Control.Exception
import Control.Applicative

(++) :: Monoid a => a -> a -> a
(++) = mappend

data SockjsRequest a = SockjsRequest { unSockjsRequest :: [a] }

instance FromJSON a => FromJSON (SockjsRequest a) where
    parseJSON js@(Array _) = SockjsRequest <$> parseJSON js
    parseJSON js = SockjsRequest . (:[]) <$> parseJSON js

data SockjsMessage = SockjsOpen
                   | SockjsHeartbeat
                   | SockjsData [ByteString]
                   | SockjsClose Int ByteString

renderSockjs :: SockjsMessage -> Builder
renderSockjs msg = case msg of
    SockjsOpen -> B.fromByteString "o"
    SockjsHeartbeat -> B.fromByteString "h"
    (SockjsData xs) -> B.fromByteString "a"
                    ++ (fromValue . toJSON $ xs)
                    ++ B.fromByteString ""
    (SockjsClose code reason) -> B.fromByteString "c["
                              ++ B.fromString (show code)
                              ++ B.fromByteString ",\""
                              ++ B.fromByteString reason
                              ++ B.fromByteString "\"]"

decodeValue :: (FromJSON a) => L.ByteString -> Maybe a
decodeValue s = case L.parse value s of
             L.Done _ v -> case fromJSON v of
                             Success a -> Just a
                             _         -> Nothing
             _          -> Nothing

decodeSockjs :: L.ByteString -> Maybe SockjsMessage
decodeSockjs s = case L.uncons s of
    Just ('o', _) -> Just SockjsOpen 
    Just ('h', _) -> Just SockjsHeartbeat 
    Just ('a', s') -> SockjsData <$> decodeValue s'
    Just ('c', s') -> do (code, reason) <- decodeValue s'
                         return $ SockjsClose code reason
    _ -> trace "unknown message." Nothing

data SockjsException = SockjsReadEOF
                     | SockjsInvalidJson
                     | SockjsError String
    deriving (Show, Typeable)

instance Exception SockjsException
