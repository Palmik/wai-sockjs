{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Network.Wai.Sock.Handler
( sock
) where

------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Applicative
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
import qualified Data.Text            as TS (Text, isPrefixOf, isSuffixOf)
------------------------------------------------------------------------------
import qualified Network.Wai        as W (Application, Request(..), Response(..))
import           Network.Wai.Extra
------------------------------------------------------------------------------
import           Network.Wai.Sock.Application
import           Network.Wai.Sock.Environment
import           Network.Wai.Sock.Request
import           Network.Wai.Sock.Server
import           Network.Wai.Sock.Transport
import           Network.Wai.Sock.Transport.XHR
------------------------------------------------------------------------------

-- TODO: ServerSettings, Environment and ([TS.Text] -> Maybe (Application m)) should be part of Server monad.

sock :: ServerSettings
     -> Environment
     -> ([TS.Text] -> Maybe (Application (C.ResourceT IO)))
     -> W.Application
sock set env find req =
    maybe (return response404) run . find $ W.pathInfo req
    where run app = runServer (handleSubroutes app req) ServerState
                        { serverSettings = set
                        , serverEnvironment = env
                        , serverApplicationRouter = find
                        }

handleSubroutes :: Application (C.ResourceT IO)
                -> W.Request
                -> Server W.Response
handleSubroutes app req =
    case (W.requestMethod req, suffix) of
        -- TODO: Add OPTIONS response.
        ("GET", [])          -> return responseGreeting
        ("GET", [""])        -> return responseGreeting
        ("GET", ["info"])    -> responseInfo
        ("GET", [r])
            | isIframe r     -> responseIframe req
        (_, [_, sid, trans]) -> handleTransport trans (Request req sid app)
        _                    -> return response404
                                                           
    where suffix     = drop (length . applicationPrefix $ applicationSettings app) $ W.pathInfo req
          isIframe p = TS.isPrefixOf "iframe" p && TS.isSuffixOf ".html" p


handleTransport :: TS.Text
                -> Request
                -> Server W.Response
handleTransport trans req =
    case trans of
        "websocket"     -> return response404
        "xhr"           -> handle (Proxy :: Proxy XHRPolling)
        "xhr_send"      -> handle (Proxy :: Proxy XHRSend)
        "xhr_streaming" -> return response404
        "eventsource"   -> return response404
        "htmlfile"      -> return response404
        "jsonp"         -> return response404
        "jsonp_send"    -> return response404
        _               -> return response404
    where handle tag = handleIncoming tag req
 
------------------------------------------------------------------------------
-- | Standard responses (greeting, info, iframe)

responseGreeting :: W.Response
responseGreeting = response200 headerPlain "Welcome to SockJS!\n"

responseIframe :: W.Request
               -> Server W.Response
responseIframe req = go . convertTS2BL . serverSettingsSockURL <$> getServerSettings
    where
      go url = case lookup "If-None-Match" (W.requestHeaders req) of
                    (Just s) | s == hashed -> response304
                    _                      -> response200 headers content
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
          headers = headerHTML ++ headerCache ++ headerETag hashed

responseInfo :: Server W.Response
responseInfo = do
    ent <- liftIO $ randomRIO ((0, 4294967295) :: (Int, Int))
    ssWS <- serverSettingsWebsocketsEnabled <$> getServerSettings
    ssCN <- serverSettingsCookiesNeeded <$> getServerSettings
    ssOR <- serverSettingsAllowedOrigins <$> getServerSettings
    return . response200 headerJSON . AE.encode $ AE.object
        [ "websocket"     .= ssWS
        , "cookie_needed" .= ssCN
        , "origins"       .= ssOR
        , "entropy"       .= ent
        ]
