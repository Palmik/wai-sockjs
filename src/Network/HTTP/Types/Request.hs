module Network.HTTP.Types.Request
( IsRequest(..)
, requestBodyConsumed
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, fromChunks)
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as C
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------
import           Network.HTTP.Types
------------------------------------------------------------------------------

class IsRequest req where
    requestMethod  :: req -> Method
    requestHeaders :: req -> RequestHeaders
    requestQuery   :: req -> Query
    requestPath    :: req -> [TS.Text]
    requestBody    :: req -> C.Source (C.ResourceT IO) BS.ByteString

requestBodyConsumed :: IsRequest req => req -> C.ResourceT IO BL.ByteString
requestBodyConsumed req = BL.fromChunks <$> (requestBody req C.$$ C.consume)
