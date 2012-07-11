module Network.Wai.Sock
( sock
, intercept
, runSockServer
) where

------------------------------------------------------------------------------
import qualified Data.ByteString        as BS (ByteString)
import qualified Data.Conduit           as C
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Request     as H
import qualified Network.HTTP.Types.Response    as H
import qualified Network.Wai                    as W (Request(..), Response(..))
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
                      --, W.settingsIntercept = intercept state
                      }
                 (sock state)

intercept :: S.ServerState
          -> W.Request
          -> Maybe (C.Source (C.ResourceT IO) BS.ByteString -> W.Connection -> C.ResourceT IO ())
intercept state = WS.intercept $ S.sockWS state

sock :: S.ServerState
     -> W.Request
     -> C.ResourceT IO W.Response
sock state r = S.runServer (S.sock r) state

instance H.IsResponse W.Response where
    responseBuilder = W.ResponseBuilder
    responseSource = W.ResponseSource

instance H.IsRequest W.Request where
    requestMethod  = W.requestMethod
    requestHeaders = W.requestHeaders
    requestQuery   = W.queryString
    requestPath    = W.pathInfo
    requestBody    = W.requestBody