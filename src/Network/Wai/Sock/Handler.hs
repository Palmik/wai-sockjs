{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE TupleSections     #-}

module Network.Wai.Sock.Handler
( sock
, runSockServer
) where

------------------------------------------------------------------------------
import           System.Random (randomRIO)
------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.MVar.Lifted
import           Control.Concurrent.MVar.Extra.Lifted (mvar)
import           Control.Monad.IO.Class
------------------------------------------------------------------------------
import           Data.Aeson                 ((.=))
import qualified Data.Aeson           as AE (ToJSON(..), encode, object)
import qualified Data.Binary          as BI (encode)
import qualified Data.ByteString.Lazy as LB (ByteString, toChunks)
import qualified Data.ByteString      as SB (ByteString, empty)
import qualified Data.Conduit         as C  (ResourceT)
import           Data.Digest.Pure.MD5       (md5)
import           Data.HashMap.Lazy          (HashMap)
import qualified Data.HashMap.Lazy    as HM
import           Data.Int                   (Int64)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text            as ST (Text, isPrefixOf, isSuffixOf)
------------------------------------------------------------------------------
import qualified Blaze.ByteString.Builder            as B (Builder)
import qualified Blaze.ByteString.Builder.ByteString as B (fromLazyByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8  as B (fromString, fromLazyText)
------------------------------------------------------------------------------
import qualified Network.HTTP.Types       as H
import qualified Network.Wai              as W
import qualified Network.Wai.Handler.Warp as W
------------------------------------------------------------------------------
import           Network.Wai.Sock.Session
import           Network.Wai.Sock.Frame
import           Network.Wai.Sock.Transport
import           Network.Wai.Sock.Transport.WebSocket
------------------------------------------------------------------------------

type SessionMap = HashMap SessionID (MVar Session)

runSockServer :: W.Port -> ([ST.Text] -> Maybe (Application, [ST.Text], [ST.Text])) -> IO ()
runSockServer p r = do
    mvsm <- newMVar HM.empty
    W.runSettings W.defaultSettings { W.settingsPort = p } (sock mvsm r)

sock :: MVar SessionMap -> ([ST.Text] -> Maybe (Application, [ST.Text], [ST.Text])) -> W.Application
sock mvsm r req = do
    maybe (return response404) run $ r $ W.pathInfo req
    where run (app, prefix, suffix) = runApplication mvsm app suffix req

runApplication :: MVar SessionMap -> Application -> [ST.Text] -> W.Application
runApplication mvsm app suffix req =
    case requestInfo  of
        -- TODO: Add OPTIONS response.
        -- TODO: Make url configurable (part of application's options).
        ("GET", [])          -> return responseGreeting
        ("GET", [""])        -> return responseGreeting
        ("GET", ["info"])    -> flip responseInfo req <$> liftIO (randomRIO (0, 4294967295))
        ("GET", [r])
            | iframeRoute r  -> return $ responseIframe url req
        (_, [_, sid, trans]) -> transport trans sid mvsm app req
        _                    -> return response404
    where requestInfo = (W.requestMethod req, suffix)
          iframeRoute p = ST.isPrefixOf "iframe" p && ST.isSuffixOf ".html" p
          url = "http://cdn.sockjs.org/sockjs-0.3.min.js"

transport :: ST.Text
          -> SessionID 
          -> MVar SessionMap
          -> Application
          -> W.Request
          -> C.ResourceT IO W.Response
transport t sid mvsm app req@(W.Request {..}) =
    case t of
        "websocket"     -> transportHandler (Proxy :: Proxy WebSocket) sid mvsm app req
        "xhr"           -> (liftIO . putStrLn) "transport xhr" >> undefined
        "xhr_send"      -> (liftIO . putStrLn) "transport xhr_send" >> undefined
        "xhr_streaming" -> (liftIO . putStrLn) "transport xhr_streaming" >> undefined
        "eventsource"   -> (liftIO . putStrLn) "transport eventsource" >> undefined
        "htmlfile"      -> (liftIO . putStrLn) "transport htmlfile" >> undefined
        "jsonp"         -> (liftIO . putStrLn) "transport jsonp" >> undefined
        "jsonp_send"    -> (liftIO . putStrLn) "transport jsonp_send" >> undefined
        _               -> return response404

transportHandler :: Transport tag
                 => Proxy tag
                 -> SessionID
                 -> MVar SessionMap
                 -> Application
                 -> W.Request
                 -> C.ResourceT IO W.Response
transportHandler pt sid mvsm app req = liftIO (getSession sid mvsm) >>= process
    where getSession :: SessionID -> MVar SessionMap -> IO (MVar Session)
          getSession s mvsm' = modifyMVar mvsm' $ \sm ->
              case HM.lookup s sm of
                   Nothing -> do -- ^ There is no session with this ID, so we create one.
                       sd <- newSessionData sid
                       ms <- newMVar $ Session (newSession pt) sd
                       return (HM.insert sid ms sm, ms)
                   Just x  -> return (sm, x) -- ^ There already exists a sessions with this ID.


          process :: MVar Session -> C.ResourceT IO W.Response
          process ms = mvar
                           (return $ frameResponse pt $ CloseFrame 2010 "Another connection still open")
                           (\x@(Session ts ds) ->
                               if not $ sessClosed ds
                                 then if sessInitialized ds
                                        then (x,) <$> (runSession ts app) -- TODO: We should check that the payload is not empty.
                                        else return $ (x, frameResponse pt OpenFrame) -- TODO: Should we check that the payload is not empty here? (http://sockjs.github.com/sockjs-protocol/sockjs-protocol-0.3.html#section-45)
                                        else return (x, frameResponse pt $ CloseFrame 3000 "Go away!")
                           ) ms

------------------------------------------------------------------------------
-- | Standard responses (greeting, info, iframe)

responseGreeting :: W.Response
responseGreeting = response200 headerPlain $ B.fromLazyByteString "Welcome to SockJS!\n"

responseIframe :: LB.ByteString -> W.Request -> W.Response
responseIframe url req =
    case lookup "If-None-Match" (W.requestHeaders req) of
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
          \  <script src=\"" <> url <> "\"></script>\n\
          \</head>\n\
          \<body>\n\
          \  <h2>Don't panic!</h2>\n\
          \  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>\n\
          \</body>\n\
          \</html>"
      hashed = toStrict . BI.encode $ md5 content
      headers = headerHTML ++ headerCache ++ headerETag hashed

data InfoResponse = InfoResponse
    { irWebsocketsEnabled :: Bool
    , irCookiesNeeded :: Bool
    , irOrigins :: String
    , irEntropy :: Int64
    }

instance AE.ToJSON InfoResponse where
    toJSON InfoResponse{..} = AE.object
        [ "websocket"     .= irWebsocketsEnabled
        , "cookie_needed" .= irCookiesNeeded
        , "origins"       .= irOrigins
        , "entropy"       .= irEntropy
        ]

responseInfo :: Int64    -- The entropy
             -> W.Request
             -> W.Response
responseInfo ent req = response200 headerJSON . B.fromLazyByteString $ AE.encode InfoResponse
    { irWebsocketsEnabled = True
    , irCookiesNeeded = True
    , irOrigins = "*:*"
    , irEntropy = ent
    }

------------------------------------------------------------------------------
-- | Response Helpers
        
response404 :: W.Response
response404 = W.ResponseBuilder H.status404 headerPlain mempty

response200 :: H.ResponseHeaders -> B.Builder -> W.Response
response200 = W.ResponseBuilder H.status200

response304 :: W.Response
response304 = W.ResponseBuilder H.status304 [] mempty

------------------------------------------------------------------------------
-- | Headers

headerPlain :: H.ResponseHeaders
headerPlain = [("Content-Type", "text/plain; charset=UTF-8")]

headerHTML :: H.ResponseHeaders
headerHTML = [("Content-Type", "text/html; charset=UTF-8")]

headerJSON :: H.ResponseHeaders
headerJSON = [("Content-Type", "application/json; charset=UTF-8")]

headerCache :: H.ResponseHeaders
headerCache = [("Cache-Control", "public; max-age=31536000;"),("Expires", "31536000")]

headerETag :: H.Ascii -> H.ResponseHeaders
headerETag etag = [("ETag", etag)]

------------------------------------------------------------------------------
-- | General Helpers

toStrict :: LB.ByteString -> SB.ByteString
toStrict = fromMaybe SB.empty . listToMaybe . LB.toChunks