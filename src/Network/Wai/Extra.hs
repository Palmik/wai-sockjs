{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Extra
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

, method
, methods
) where

------------------------------------------------------------------------------
import qualified Data.ByteString      as BS (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           Data.Monoid                (mempty, (<>))
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H
import qualified Network.Wai        as W (Request(..), Response(..), responseLBS)
import           Web.Cookie
------------------------------------------------------------------------------

method :: Monad m => H.Method -> W.Request -> m a -> (W.Request -> m a) -> m a
method m req ac f
    | m == W.requestMethod req = f req
    | otherwise                = ac

methods :: Monad m => [H.Method] -> W.Request -> m a -> (W.Request -> m a) -> m a
methods ms req ac f
    | W.requestMethod req `elem` ms = f req
    | otherwise                     = ac

------------------------------------------------------------------------------
-- | Response utility functions.

response200 :: H.ResponseHeaders -> BL.ByteString -> W.Response
response200 = W.responseLBS H.status200

response204 :: H.ResponseHeaders -> BL.ByteString -> W.Response
response204 = W.responseLBS H.status204

response304 :: W.Response
response304 = W.responseLBS H.status304 [] mempty

response404 :: W.Response
response404 = W.responseLBS H.status404 headerPlain mempty

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

headerJSESSIONID :: W.Request -> H.ResponseHeaders
headerJSESSIONID req = [("Set-Cookie", "JSESSIONID=" <> jsessionID <> "; path=/")]
    where jsessionID = fromMaybe "dummy" $
                           lookup "Cookie" (W.requestHeaders req) >>=
                           lookup "JSESSIONID" . parseCookies

headerCORS :: BS.ByteString -> W.Request -> H.ResponseHeaders
headerCORS def req = allowHeaders ++ allowOrigin ++ allowCredentials
    where allowCredentials = [("Access-Control-Allow-Credentials", "true")]
          allowHeaders =
              case lookup "Access-Control-Request-Headers" $ W.requestHeaders req of
                   Just "" -> []
                   Just ah -> [("Access-Control-Allow-Headers", ah)]
                   Nothing -> []
          allowOrigin =
              case origin of
                   ""     -> []
                   "null" -> [("Access-Control-Allow-Origin", def)]
                   _      -> [("Access-Control-Allow-Origin", origin)]
          origin = fromMaybe def . lookup "Origin" $ W.requestHeaders req

