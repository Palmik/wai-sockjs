{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Types.Extra
( response200
, response204
, response304
, response404

, headerCached
, headerNotCached
, headerCORS
, headerETag
, headerHTML
, headerJSON
, headerJSESSIONID
, headerJS
, headerPlain
) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.Monoid                (mempty, (<>))
------------------------------------------------------------------------------
import qualified Network.HTTP.Types          as H
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
import           Web.Cookie                  as H
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Response utility functions.

response200 :: H.ResponseHeaders -> BL.ByteString -> H.Response
response200 = H.response H.status200

response204 :: H.ResponseHeaders -> BL.ByteString -> H.Response
response204 = H.response H.status204

response304 :: H.Response
response304 = H.response H.status304 [] mempty

response404 :: H.Response
response404 = H.response H.status404 headerPlain mempty

------------------------------------------------------------------------------
-- | Header utility functions.

headerCached :: H.ResponseHeaders
headerCached = [("Cache-Control", "public; max-age=31536000;"),("Expires", "31536000"), ("Access-Control-Max-Age", "31536000")]

headerNotCached :: H.ResponseHeaders
headerNotCached = [("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")]

headerETag :: H.Ascii -> H.ResponseHeaders
headerETag etag = [("ETag", etag)]

headerHTML :: H.ResponseHeaders
headerHTML = [("Content-Type", "text/html; charset=UTF-8")]

headerJSON :: H.ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerJS :: H.ResponseHeaders
headerJS = [("Content-Type", "application/javascript; charset=UTF-8")]

headerPlain :: H.ResponseHeaders
headerPlain = [("Content-Type", "text/plain; charset=UTF-8")]

headerJSESSIONID :: H.Request -> H.ResponseHeaders
headerJSESSIONID req = [("Set-Cookie", "JSESSIONID=" <> jsessionID <> "; path=/")]
    where jsessionID = fromMaybe "dummy" $
                           lookup "Cookie" (H.requestHeaders req) >>=
                           lookup "JSESSIONID" . H.parseCookies

headerCORS :: BS.ByteString -> H.Request -> H.ResponseHeaders
headerCORS def req = allowHeaders ++ allowOrigin ++ allowCredentials
    where allowCredentials = [("Access-Control-Allow-Credentials", "true")]
          allowHeaders =
              case lookup "Access-Control-Request-Headers" $ H.requestHeaders req of
                   Just "" -> []
                   Just ah -> [("Access-Control-Allow-Headers", ah)]
                   Nothing -> []
          allowOrigin =
              case origin of
                   ""     -> []
                   "null" -> [("Access-Control-Allow-Origin", def)]
                   _      -> [("Access-Control-Allow-Origin", origin)]
          origin = fromMaybe def . lookup "Origin" $ H.requestHeaders req

