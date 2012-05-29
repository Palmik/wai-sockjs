module Network.Wai.Sock.Handler
(
) where

------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
------------------------------------------------------------------------------
import qualified Data.Aeson           as AE (encode)
import qualified Data.Binary          as BI (encode)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString      as SB (ByteString, empty)
import           Data.Digest.Pure.MD5       (md5)
import           Data.Int                   (Int64)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as ST (Text, isPrefixOf, isSuffixOf)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder.ByteString as B (fromLazyByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8  as B (fromString, fromLazyText)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H
import qualified Network.Wai        as W (Application)
------------------------------------------------------------------------------
import           Network.Wai.Sock.Application
------------------------------------------------------------------------------

handler :: Monad m => (ST.Text -> Application m) -> W.Application
handler = undefined