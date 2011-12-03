{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getArgs)

import Data.Maybe
import qualified Data.Map as M

import Control.Applicative
import Control.Concurrent

import Network.Wai
import Network.Wai.Handler.Warp
import Network.WebSockets (TextProtocol)

import Data.FileEmbed (embedDir)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import Sockjs
import Apps (echo, chat, close, ServerState, chat)

serverState :: TextProtocol p => MVar (ServerState p)
serverState = unsafePerformIO $ newMVar M.empty

staticApp :: Application
staticApp = Static.staticApp Static.defaultFileServerSettings
              { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static") }
              -- { Static.ssFolder = Static.fileSystemLookup "static" }

wsApps :: TextProtocol p => AppRoute p
wsApps = [ ( ["echo"],                    (echo,             Nothing) )
         , ( ["chat"],                    (chat serverState, Nothing) )
         , ( ["close"],                   (close,            Nothing) )
         , ( ["disabled_websocket_echo"], (echo,             Just ["websocket"]) )
         ]

main :: IO ()
main = do
    port <- read . fromMaybe "8080" . listToMaybe <$> getArgs
    msm <- newMVar M.empty
    putStrLn $ "http://localhost:"++show port++"/static/client.html"
    runSettings defaultSettings
           { settingsPort = port
           , settingsIntercept = WaiWS.intercept (wsRoute wsApps)
           } $ httpRoute [(["static"], staticApp)] (sockjsRoute msm wsApps)

