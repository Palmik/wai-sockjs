module Network.HTTP.Types.Request
( Request(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------
import           Network.HTTP.Types
------------------------------------------------------------------------------

data Request = Request
    { requestMethod :: Method
    , requestHeaders :: RequestHeaders
    , requestPath :: [TS.Text]
    , requestBody :: BL.ByteString
    }
