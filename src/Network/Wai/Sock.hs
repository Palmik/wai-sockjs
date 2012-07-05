module Network.Wai.Sock
( sock
, intercept
, runSockServer
) where

------------------------------------------------------------------------------
import           Control.Applicative
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy   as BL (fromChunks)
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.Conduit           as C
import qualified Data.Conduit.List      as C
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Request     as H
import qualified Network.HTTP.Types.Response    as H
import qualified Network.Wai                    as W (Request(..), Response(..), responseLBS)
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as WS
------------------------------------------------------------------------------
import qualified Network.Sock.Handler as S
import qualified Network.Sock.Server  as S
------------------------------------------------------------------------------

runSockServer :: W.Port -> S.ServerState -> IO ()
runSockServer port state = do
    W.runSettings W.defaultSettings
                      { W.settingsPort = port
                      , W.settingsIntercept = intercept state
                      }
                 (sock state)

intercept :: S.ServerState
          -> W.Request
          -> Maybe (C.Source (C.ResourceT IO) BS.ByteString -> W.Connection -> C.ResourceT IO ())
intercept state = WS.intercept $ S.sockWS state

sock :: S.ServerState
     -> W.Request
     -> C.ResourceT IO W.Response
sock state r = do
    req <- convertRequest r
    convertResponse <$> (S.runServer (S.sock req) state)

convertRequest :: W.Request -> C.ResourceT IO H.Request
convertRequest req = do
    body <- BL.fromChunks <$> (W.requestBody req C.$$ C.consume)
    return $ H.Request
                 { H.requestBody = body
                 , H.requestHeaders = W.requestHeaders req
                 , H.requestPath = W.pathInfo req
                 , H.requestMethod = W.requestMethod req
                 }

convertResponse :: H.Response -> W.Response
convertResponse res = W.responseLBS (H.responseStatus res)
                                    (H.responseHeaders res)
                                    (H.responseBody res)