module Network.Wai.Sock.Request
( Request(..)

, requestMethod
, requestHeaders
, pathInfo
, requestBody
, consumeRequestBody
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy   as BL (ByteString, fromChunks)
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.Conduit           as C
import qualified Data.Conduit.List      as C
import qualified Data.Text              as TS (Text)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H (RequestHeaders, Method)
import qualified Network.Wai        as W (Request(..))
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (Request(..))
------------------------------------------------------------------------------

requestMethod :: Request -> H.Method
requestMethod = W.requestMethod . requestRaw

requestHeaders :: Request -> H.RequestHeaders
requestHeaders = W.requestHeaders . requestRaw

pathInfo :: Request -> [TS.Text]
pathInfo = W.pathInfo . requestRaw

requestBody :: Request -> C.Source (C.ResourceT IO) BS.ByteString
requestBody = W.requestBody . requestRaw

consumeRequestBody :: Request -> C.ResourceT IO BL.ByteString
consumeRequestBody req = BL.fromChunks <$> (requestBody req C.$$ C.consume)