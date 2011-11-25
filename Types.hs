{-# LANGUAGE ExistentialQuantification, OverloadedStrings, DeriveDataTypeable #-}
module Types
  ( SockjsMessage (..)
  , renderSockjs
  , SockjsException (..)
  , SockjsRequest (..)
  ) where

import Prelude hiding ( (++) )
import Data.Typeable
import Data.Monoid
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Aeson
import Data.Aeson.Encode
import qualified Data.Vector as V
import Control.Exception
import Control.Applicative

(++) = mappend

data SockjsRequest a = SockjsRequest { unSockjsRequest :: [a] }

instance FromJSON a => FromJSON (SockjsRequest a) where
    parseJSON js@(Array _) = SockjsRequest <$> parseJSON js
    parseJSON js = SockjsRequest . (:[]) <$> parseJSON js

data SockjsMessage = SockjsOpen
                   | SockjsHeartbeat
                   | forall a. ToJSON a => SockjsData [a]
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

decodeSockjsMessage :: L.ByteString -> Maybe SockjsMessage
decodeSockjsMessage s = case L.uncons s of
    Just ('o', _) -> Just SockjsOpen 
    Just ('a', s') -> SockjsData <$> decode s'
    Just ('c', s') -> SockjsClose <$> decode s' <*> 
    

data SockjsException = SockjsReadEOF
                     | SockjsInvalidJson
                     | SockjsError String
    deriving (Show, Typeable)

instance Exception SockjsException
