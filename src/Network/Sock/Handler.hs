{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Handler
( sock
) where

------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Monad.IO.Class
------------------------------------------------------------------------------
import qualified Data.Aeson           as AE (encode, object)
import           Data.Aeson                 ((.=))
import qualified Data.Binary          as BI (encode)
import           Data.ByteString.Extra      (convertBL2BS, convertTS2BL)
import qualified Data.Conduit         as C
import           Data.Digest.Pure.MD5       (md5)
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text            as TS (Text, isPrefixOf, isInfixOf, isSuffixOf)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Extra    as H
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
------------------------------------------------------------------------------
import           Network.Sock.Application
import           Network.Sock.Request
import           Network.Sock.Server
import           Network.Sock.Transport
import           Network.Sock.Transport.XHR
------------------------------------------------------------------------------

sock :: H.Request -> Server H.Response
sock req = do
--     maybe (return H.response404)
--           (flip handleSubroutes req)
--           (router <$> getServerApplicationRouter)
    router <- getServerApplicationRouter
    case router $ H.requestPath req of
         Just app -> handleSubroutes app req
         Nothing  -> return H.response404

handleSubroutes :: Application (C.ResourceT IO)
                -> H.Request
                -> Server H.Response
handleSubroutes app req =
    case (H.requestMethod req, suffix) of
        -- TODO: Add OPTIONS response.
        ("GET", [])           -> return responseGreeting
        ("GET", [""])         -> return responseGreeting
        ("GET",     ["info"]) -> responseInfo (applicationSettings app) req
        ("OPTIONS", ["info"]) -> return $ responseOptions ["OPTIONS", "GET"] req
        ("GET", [r])
            | isIframe r      -> return $ responseIframe (applicationSettings app) req
        (_, [srvrid, sid, trans])
            | okID srvrid && okID sid -> responseTransport trans (Request req sid app) -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-40
            | otherwise               -> return H.response404
        _                     -> return H.response404
                                                           
    where suffix     = drop (length . settingsApplicationPrefix $ applicationSettings app) $ H.requestPath req
          isIframe p = TS.isPrefixOf "iframe" p && TS.isSuffixOf ".html" p
          okID sid   = not (TS.isInfixOf "." sid || sid == "")


------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/<server_id>/<session_id>/<transport>
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-36 and the following sections.
responseTransport :: TS.Text
                  -> Request
                  -> Server H.Response
responseTransport trans req =
    case trans of
        "websocket"     -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-50
        "xhr"           -> handle (Proxy :: Proxy XHRPolling)  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-74
        "xhr_send"      -> handle (Proxy :: Proxy XHRSend)     -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-74
        "xhr_streaming" -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-83
        "eventsource"   -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-91
        "htmlfile"      -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-100
        "jsonp"         -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-108
        "jsonp_send"    -> return H.response404                  -- http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-108
        _               -> return H.response404
    where handle tag = handleIncoming tag req
 
------------------------------------------------------------------------------
-- | Used as a response to:
--
--     * http://example.com/<application_prefix>/
--     * http://example.com/<application_prefix>
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-12
responseGreeting :: H.Response
responseGreeting = H.response200 H.headerPlain "Welcome to SockJS!\n"

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/<application_prefix>/iframe*.html
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-15
responseIframe :: ApplicationSettings
               -> H.Request
               -> H.Response
responseIframe appSet req = go . convertTS2BL $ settingsSockURL appSet
    where
      go url = case lookup "If-None-Match" (H.requestHeaders req) of
                    (Just s) | s == hashed -> H.response304
                    _                      -> H.response200 headers content
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
              \  <script src=\"" <> url <> "\"></script>\n\
              \</head>\n\
              \<body>\n\
              \  <h2>Don't panic!</h2>\n\
              \  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
              \</body>\n\
              \</html>"
          hashed  = convertBL2BS . BI.encode $ md5 content
          headers = H.headerHTML <> H.headerCached <> H.headerETag hashed

------------------------------------------------------------------------------
-- | Used as a response to http://example.com/info
--
--   Documentation: http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-26
responseInfo :: ApplicationSettings
             -> H.Request
             -> Server H.Response
responseInfo appSet req = do
    ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
    return . H.response200 (H.headerJSON <> H.headerNotCached <> H.headerCORS "*" req) . AE.encode $ AE.object
        [ "websocket"     .= settingsWebsocketsEnabled appSet
        , "cookie_needed" .= settingsCookiesNeeded     appSet
        , "origins"       .= settingsAllowedOrigins    appSet
        , "entropy"       .= ent
        ]
