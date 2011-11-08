{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.IO.Class
import Data.List (isPrefixOf)
import Control.Applicative
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Network.WebSockets as WS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S
import Network.Wai.Application.Static
import Network.Socket
import qualified Data.Enumerator as E

main = runSettings defaultSettings
         { settingsPort = 8080
         , settingsIntercept = sockjsIntercept
         } (staticApp defaultFileServerSettings)

sockjsIntercept :: Request -> Maybe (Socket -> E.Iteratee ByteString IO ())
sockjsIntercept req = app "/echo" sockjsEcho
                  -- <|> sockjsClose req "/close"
                  -- <|> sockjsDisable req "/disabled_websocket_echo"
  where
    app path app =
       if decodePathSegments path `isPrefixOf` pathInfo req
         then Just app
         else Nothing

sockjsEcho :: Socket -> E.Iteratee ByteString IO ()
sockjsEcho sock = do
    liftIO $ send sock "200 ok"
    return ()
