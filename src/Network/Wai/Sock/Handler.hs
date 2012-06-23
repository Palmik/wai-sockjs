{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Sock.Handler
(
) where

------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
------------------------------------------------------------------------------
import qualified Data.Aeson           as AE (encode, object)
import           Data.Aeson                 ((.=))
import qualified Data.Binary          as BI (encode)
import qualified Data.ByteString.Lazy as BL (ByteString, toChunks, fromChunks)
import qualified Data.ByteString      as BS (ByteString, empty, concat)
import           Data.Digest.Pure.MD5       (md5)
import           Data.Int                   (Int64)
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text            as TS (Text, isPrefixOf, isSuffixOf)
import qualified Data.Text.Encoding   as TS (encodeUtf8)
------------------------------------------------------------------------------
import           Blaze.ByteString.Builder                 (Builder)
import qualified Blaze.ByteString.Builder.ByteString as B (fromLazyByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8  as B (fromString, fromLazyText)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types as H
import qualified Network.Wai        as W (Application, Request(..), Response(..), responseLBS)
import           Network.Wai.Extra
------------------------------------------------------------------------------
import           Network.Wai.Sock.Application
import           Network.Wai.Sock.Environment
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Server
import           Network.Wai.Sock.Session
import           Network.Wai.Sock.Transport
import           Network.Wai.Sock.Transport.WebSocket
------------------------------------------------------------------------------

-- TODO: ServerSettings, Environment and ([TS.Text] -> Maybe (Application m)) should be part of Server monad.

sock :: ServerSettings
     -> Environment
     -> ([TS.Text] -> Maybe (Application m))
     -> W.Application
sock set mvsm find req = undefined

handleSubroutes :: ServerSettings
                -> Environment
                -> Application m
                -> W.Application
handleSubroutes set@ServerSettings{..} env app@Application{..} req =
    case (W.requestMethod req, suffix) of
        -- TODO: Add OPTIONS response.
        ("GET", [])          -> return responseGreeting
        ("GET", [""])        -> return responseGreeting
        ("GET", ["info"])    -> responseInfo set <$> liftIO (randomRIO (0, 4294967295))
        ("GET", [r])
            | isIframe r       -> return $ responseIframe set req
        (_, [_, sid, trans]) -> handleTransport trans set env app sid req
        _                    -> return response404
                                                           
    where suffix     = drop (length $ applicationPrefix applicationSettings) $ W.pathInfo req
          isIframe p = TS.isPrefixOf "iframe" p && TS.isSuffixOf ".html" p


handleTransport :: TS.Text
                -> ServerSettings
                -> Environment
                -> Application m
                -> SessionID
                -> W.Application
handleTransport trans set env app sid req =
    case trans of
        "websocket"     -> handle (Proxy :: Proxy WebSocket)
        "xhr"           -> return response404
        "xhr_send"      -> return response404
        "xhr_streaming" -> return response404
        "eventsource"   -> return response404
        "htmlfile"      -> return response404
        "jsonp"         -> return response404
        "jsonp_send"    -> return response404
        _               -> return response404
    where handle tag = handleIncoming tag env req >> return response404 -- ^ Run the application here.
 
------------------------------------------------------------------------------
-- | Standard responses (greeting, info, iframe)

responseGreeting :: W.Response
responseGreeting = response200 headerPlain "Welcome to SockJS!\n"

responseIframe :: ServerSettings -- ^ Server Settings
               -> W.Request
               -> W.Response
responseIframe ServerSettings{..} req =
    case lookup "If-None-Match" (W.requestHeaders req) of
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
          \  <script src=\"" <> convertTS2BL serverSettingsSockURL <> "\"></script>\n\
          \</head>\n\
          \<body>\n\
          \  <h2>Don't panic!</h2>\n\
          \  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
          \</body>\n\
          \</html>"
      hashed = convertBL2BS . BI.encode $ md5 content
      headers = headerHTML ++ headerCache ++ headerETag hashed

responseInfo :: ServerSettings -- ^ Server Settings
             -> Int64          -- ^ Entropy
             -> W.Response
responseInfo ServerSettings{..} ent = response200 headerJSON . AE.encode $ AE.object
    [ "websocket"     .= serverSettingsWebsocketsEnabled
    , "cookie_needed" .= serverSettingsCookiesNeeded
    , "origins"       .= serverSettingsAllowedOrigins
    , "entropy"       .= ent
    ]

------------------------------------------------------------------------------
-- | Other utility functions.

convertBS2BL :: BS.ByteString -> BL.ByteString
convertBS2BL = BL.fromChunks . (:[])

convertBL2BS :: BL.ByteString -> BS.ByteString
convertBL2BS = BS.concat . BL.toChunks

convertTS2BL :: TS.Text -> BL.ByteString
convertTS2BL = convertBS2BL . TS.encodeUtf8
