{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE GADTs             #-}

module Network.Wai.Application.Sock
( dispatcher
) where
    
------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
------------------------------------------------------------------------------
import qualified Data.Aeson           as AE (encode)
import qualified Data.Binary          as BI (encode)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString      as SB (ByteString, empty)
import           Data.Digest.Pure.MD5       (md5)
import           Data.Int                   (Int64)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as ST (isPrefixOf, isSuffixOf)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder.ByteString as B (fromLazyByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8  as B (fromString, fromLazyText)
------------------------------------------------------------------------------
import           Network.HTTP.Types
import           Network.Wai
------------------------------------------------------------------------------
import           Network.Wai.Application.Sock.Types
------------------------------------------------------------------------------

dispatcher :: Application
dispatcher req = case requestInfo of
    ("GET", [])                 -> return responseGreeting
    
    ("GET", [""])               -> return responseGreeting
    
    ("GET", [isIframe -> True]) -> return $ responseIframe req
    
    ("GET", ["info"])           -> flip responseInfo req <$>
                                   liftIO (randomRIO (0, 4294967295))
                                   
    _                           -> return response404
    where requestInfo = (requestMethod req, pathInfo req)
          isIframe p = ST.isPrefixOf "iframe" p && ST.isSuffixOf ".html" p

------------------------------------------------------------------------------
-- | Responses

responseGreeting :: Response
responseGreeting = response200 headerPlain $ B.fromLazyByteString "Welcome to SockJS!\n"

responseIframe :: Request -> Response
responseIframe req = 
    case lookup "If-None-Match" (requestHeaders req) of
        (Just s) | s == hashed -> response304
        _                      -> response200 headers $ B.fromLazyByteString content
    where
      content =
          "<!DOCTYPE html>\n\
          \<html>\n\
          \<head>\n\
          \  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />\n\
          \  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />\n\
          \  <script>\n\
          \    document.domain = document.domain;\n\
          \    _sockjs_onload = function(){SockJS.bootstrap_iframe();};\n\
          \  </script>\n\
          \  <script src=\"/static/sockjs.js\"></script>\n\
          \</head>\n\
          \<body>\n\
          \  <h2>Don't panic!</h2>\n\
          \  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
          \</body>\n\
          \</html>"
      hashed = toStrict . BI.encode $ md5 content
      headers = headerHTML ++ headerCache ++ headerETag hashed

responseInfo :: Int64    -- The entropy
             -> Request 
             -> Response
responseInfo ent req = response200 headerJSON . B.fromLazyByteString $ AE.encode ResponseInfo
    { riWebsocketsEnabled = True
    , riCookiesNeeded = True
    , riOrigins = "*:*"
    , riEntropy = ent
    }

response404 :: Response
response404 = ResponseBuilder status404 headerPlain mempty

------------------------------------------------------------------------------
-- | Response Helpers

response200 :: ResponseHeaders -> Builder -> Response
response200 = ResponseBuilder status200

response304 :: Response
response304 = ResponseBuilder status304 [] mempty

------------------------------------------------------------------------------
-- | Headers

headerPlain :: ResponseHeaders
headerPlain = [("Content-Type", "text/plain; charset=UTF-8")]

headerHTML :: ResponseHeaders
headerHTML = [("Content-Type", "text/html; charset=UTF-8")]

headerJSON :: ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerCache :: ResponseHeaders
headerCache = [("Cache-Control", "public; max-age=31536000;"),("Expires", "31536000")]

headerETag :: Ascii -> ResponseHeaders
headerETag etag = [("ETag", etag)]

------------------------------------------------------------------------------
-- | General Helpers

toStrict :: LB.ByteString -> SB.ByteString
toStrict = fromMaybe SB.empty . listToMaybe . LB.toChunks
