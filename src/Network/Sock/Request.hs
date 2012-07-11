module Network.Sock.Request
( Request(..)

, requestMethod
, requestHeaders
, requestPath
, requestQuery
, requestBody
, requestBodyConsumed
) where

------------------------------------------------------------------------------
import           Network.HTTP.Types.Request
------------------------------------------------------------------------------
import           Network.Sock.Types.Request
------------------------------------------------------------------------------

instance IsRequest Request where
    requestMethod  (Request raw _ _) = requestMethod raw
    requestHeaders (Request raw _ _) = requestHeaders raw
    requestQuery   (Request raw _ _) = requestQuery raw
    requestPath    (Request raw _ _) = requestPath raw
    requestBody    (Request raw _ _) = requestBody raw

