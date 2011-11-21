{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
import Prelude hiding ( (++) )
import Data.Monoid
import Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as B
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.Aeson
import Data.Aeson.Encode
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import Control.Applicative

(++) = mappend

data SockjsMessage = SockjsOpen
                   | SockjsHeartbeat
                   | forall a. ToJSON a => SockjsData [a]
                   | SockjsClose Int ByteString

renderMessage :: SockjsMessage -> Builder
renderMessage msg = case msg of
    SockjsOpen -> B.fromByteString "o\n"
    SockjsHeartbeat -> B.fromByteString "h\n"
    (SockjsData xs) -> B.fromByteString "a"
                    ++ (fromValue . toJSON $ xs)
                    ++ B.fromByteString "\n"
    (SockjsClose code reason) -> B.fromByteString "c["
                              ++ B.fromString (show code)
                              ++ B.fromByteString ",\""
                              ++ B.fromByteString reason
                              ++ B.fromByteString "\"]\n"

data SockjsException = SockjsReadEOF
                     | SockjsInvalidJson
                     | SockjsError String

data SockjsRequest = forall a. FromJSON a => SockjsRequest [a]

instance FromJSON SockjsRequest where
    parseJSON (Array arr) = SockjsRequest <$> mapM parseJSON (V.toList arr)
    parseJSON _ = fail "parse SockjsRequest fail."

