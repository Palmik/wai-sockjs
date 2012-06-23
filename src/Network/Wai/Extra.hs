{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Extra
( response200
, response304
, response404

, headerCache
, headerETag
, headerHTML
, headerJSON
, headerPlain

, method
, methods
) where

------------------------------------------------------------------------------
import           Control.Monad  (when)
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid                (mempty)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H
import qualified Network.Wai        as W (Application, Request(..), Response(..), responseLBS)
------------------------------------------------------------------------------

method :: Monad m => H.Method -> W.Request -> (W.Request -> m ()) -> m ()
method m req f = when (m == W.requestMethod req) $ f req

methods :: Monad m => [H.Method] -> W.Request -> (W.Request -> m ()) -> m ()
methods ms req f = when (W.requestMethod req `elem` ms) $ f req

------------------------------------------------------------------------------
-- | Response utility functions.

response200 :: H.ResponseHeaders -> BL.ByteString -> W.Response
response200 = W.responseLBS H.status200

response304 :: W.Response
response304 = W.responseLBS H.status304 [] mempty

response404 :: W.Response
response404 = W.responseLBS H.status404 headerPlain mempty

------------------------------------------------------------------------------
-- | Header utility functions.

headerCache :: H.ResponseHeaders
headerCache = [("Cache-Control", "public; max-age=31536000;"),("Expires", "31536000")]

headerETag :: H.Ascii -> H.ResponseHeaders
headerETag etag = [("ETag", etag)]

headerHTML :: H.ResponseHeaders
headerHTML = [("Content-Type", "text/html; charset=UTF-8")]

headerJSON :: H.ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerPlain :: H.ResponseHeaders
headerPlain = [("Content-Type", "text/plain; charset=UTF-8")]

