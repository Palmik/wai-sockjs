module Network.Sock.Request
( Request(..)

, requestMethod
, requestHeaders
, requestPath
, requestBody
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
import qualified Network.HTTP.Types         as H (RequestHeaders, Method)
import qualified Network.HTTP.Types.Request as H
------------------------------------------------------------------------------
import           Network.Sock.Types.Request
------------------------------------------------------------------------------

requestMethod :: Request -> H.Method
requestMethod = H.requestMethod . requestRaw

requestHeaders :: Request -> H.RequestHeaders
requestHeaders = H.requestHeaders . requestRaw

requestPath :: Request -> [TS.Text]
requestPath = H.requestPath . requestRaw

requestBody :: Request -> BL.ByteString
requestBody = H.requestBody . requestRaw

