{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
import System.Environment (getArgs)
import Data.Maybe
import Control.Applicative

import Network.Wai (Application)
import Network.Wai.Handler.Warp

import Data.FileEmbed (embedDir)
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Application.Static as Static

import Network.Wai.Application.Sockjs
import Apps (echo, chat, close', newChatState, ChatState, chat)

staticApp :: Application
staticApp = Static.staticApp Static.defaultFileServerSettings
              -- { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static") }
              { Static.ssFolder = Static.fileSystemLookup "static" }

mkApps :: ChatState -> WSLiteRoute
mkApps st = [ ( ["echo"],                    (echo,    Nothing) )
            , ( ["chat"],                    (chat st, Just ["websocket"]) )
            , ( ["close"],                   (close',  Nothing) )
            , ( ["disabled_websocket_echo"], (echo,    Just ["websocket"]) )
            ]

main :: IO ()
main = do
    port <- read . fromMaybe "8080" . listToMaybe <$> getArgs
    sockjsState <- newSockjsState
    chatState <- newChatState
    putStrLn $ "http://localhost:"++show port++"/static/client.html"
    let apps = mkApps chatState
    runSettings defaultSettings
           { settingsPort = port
           , settingsIntercept = WaiWS.intercept (wsApps apps)
           } $ waiRoute [(["static"], staticApp)] (waiApps sockjsState apps)

