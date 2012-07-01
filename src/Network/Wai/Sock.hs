module Network.Wai.Sock
( sock
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.Trans.Class
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy   as BL (fromChunks)
import qualified Data.Conduit           as C
import qualified Data.Conduit.List      as C
------------------------------------------------------------------------------
import qualified Network.HTTP.Types.Request  as H
import qualified Network.HTTP.Types.Response as H
import qualified Network.Wai                 as W (Request(..), Response(..), responseLBS)
------------------------------------------------------------------------------
import qualified Network.Sock.Handler as S
import qualified Network.Sock.Server  as S
------------------------------------------------------------------------------

sock :: S.ServerState
     -> W.Request
     -> C.ResourceT IO W.Response
sock state req = S.runServer (convertRequest req >>= S.sock >>= convertResponse) state

convertRequest :: W.Request -> S.Server H.Request
convertRequest req = lift $ do
    body <- BL.fromChunks <$> (W.requestBody req C.$$ C.consume)
    return $ H.Request
                 { H.requestBody = body
                 , H.requestHeaders = W.requestHeaders req
                 , H.requestPath = W.pathInfo req
                 , H.requestMethod = W.requestMethod req
                 }

convertResponse :: H.Response -> S.Server W.Response
convertResponse res = lift $
    return $ W.responseLBS (H.responseStatus res)
                           (H.responseHeaders res)
                           (H.responseBody res)