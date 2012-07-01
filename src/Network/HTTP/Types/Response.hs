module Network.HTTP.Types.Response
( Response(..)
, response
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
------------------------------------------------------------------------------
import           Network.HTTP.Types
------------------------------------------------------------------------------

data Response = Response
    { responseStatus :: Status
    , responseHeaders :: ResponseHeaders
    , responseBody :: BL.ByteString
    }

response :: Status -> ResponseHeaders -> BL.ByteString -> Response
response = Response
 
