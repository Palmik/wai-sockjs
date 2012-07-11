module Network.HTTP.Types.Response
( IsResponse(..)
, responseLBS
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Conduit         as C  (Source, ResourceT, Flush)
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder as B
------------------------------------------------------------------------------
import           Network.HTTP.Types
------------------------------------------------------------------------------

class IsResponse res where
    responseBuilder :: Status -> ResponseHeaders -> B.Builder -> res
    responseSource :: Status -> ResponseHeaders -> (C.Source (C.ResourceT IO) (C.Flush B.Builder)) -> res

responseLBS :: IsResponse res
            => Status
            -> ResponseHeaders
            -> BL.ByteString
            -> res
responseLBS s h = responseBuilder s h . B.fromLazyByteString
 
