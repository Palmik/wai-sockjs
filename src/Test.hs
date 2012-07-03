{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
------------------------------------------------------------------------------
import qualified Data.HashMap.Strict as HM
import           Data.Conduit
import qualified Data.Conduit.List   as C
import           Data.Default
import           Data.List
import           Data.Monoid
import qualified Data.Text as TS
------------------------------------------------------------------------------
import qualified Network.Sock.Application as S
import qualified Network.Sock.Server      as S
import qualified Network.Wai.Sock         as S
import           Network.Wai.Handler.Warp as W
------------------------------------------------------------------------------

echo = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["echo"]
          }
    , S.applicationDefinition = definition
    }
    where definition source sink = source $$ sink

harrEcho = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["harr_echo"]
          }
    , S.applicationDefinition = definition
    }
    where definition source sink = source $= C.map foo $$ sink
              where foo x = "Harr! " <> x

disabledWebsocketEcho = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["disabled_websocket_echo"]
          , S.settingsWebsocketsEnabled = False
          }
    , S.applicationDefinition = definition
    }
    where definition source sink = source $$ sink

router :: [S.Application m] -> [TS.Text] -> Maybe (S.Application m)
router apps pathInfo = find (\app -> S.settingsApplicationPrefix (S.applicationSettings app) `isPrefixOf` pathInfo) apps

runSockServer :: Port -> ([TS.Text] -> Maybe (S.Application (ResourceT IO))) -> IO ()
runSockServer p r = do
    sessions <- newMVar HM.empty
    let state =
            S.ServerState
                { S.serverEnvironment =
                      S.ServerEnvironment
                          { S.environmentSessions = sessions
                          }
                , S.serverApplicationRouter = r
                , S.serverSettings = def
                }
    W.runSettings W.defaultSettings { W.settingsPort = p } (S.sock state)

main = runSockServer 8080 (router [echo, harrEcho, disabledWebsocketEcho])