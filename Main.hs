{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import Debug.Trace

import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Data.Maybe
import qualified Data.Map as M

import Control.Exception
import Control.Applicative
import Control.Concurrent

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets (TextProtocol)

import Data.FileEmbed (embedDir)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import qualified Data.Enumerator as E

import Sockjs
import Apps (echo, chat, close, ServerState, chat)

serverState :: TextProtocol p => MVar (ServerState p)
serverState = unsafePerformIO $ newMVar M.empty

staticApp :: Application
staticApp = Static.staticApp Static.defaultFileServerSettings
              { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static") }
              -- { Static.ssFolder = Static.fileSystemLookup "static" }

wsApps :: TextProtocol p => AppRoute p
wsApps = [ ( ["echo"], echo )
         , ( ["chat"], chat serverState )
         , ( ["close"], close )
         ]

catchMiddleware :: Application -> Application
catchMiddleware app req = app req `E.catchError` (\err -> trace "exc" $ return $ serverErrorRsp $ show $ (fromException err :: Maybe IOException))

main :: IO ()
main = do
    port <- read . fromMaybe "3000" . listToMaybe <$> getArgs
    msm <- newMVar M.empty
    runSettings defaultSettings
           { settingsPort = port
           , settingsIntercept = WaiWS.intercept (wsRoute wsApps)
           } $ httpRoute [(["static"], staticApp)] (catchMiddleware $ sockjsRoute msm wsApps)

