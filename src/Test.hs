{-# LANGUAGE OverloadedStrings #-}

module Main where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import           Data.Conduit
import qualified Data.Conduit.List   as C
import           Data.Default
import           Data.List
import           Data.Monoid
import qualified Data.Text as TS
------------------------------------------------------------------------------
import qualified Network.Sock             as S
import qualified Network.Sock.Application as S
import qualified Network.Sock.Server      as S
import qualified Network.Sock.Protocol    as S
import qualified Network.Wai.Sock         as S
------------------------------------------------------------------------------

close = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["close"]
          , S.settingsCookiesNeeded = True
          }
    , S.applicationDefinition = definition
    }
    where definition :: Source (ResourceT IO) BL.ByteString -> Sink S.Protocol (ResourceT IO) () -> ResourceT IO ()
          definition source sink = S.close $$ sink

echo = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["echo"]
          }
    , S.applicationDefinition = definition
    }
    where definition source sink = source $= S.mapMessage $$ sink

disabledWebsocketEcho = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["disabled_websocket_echo"]
          , S.settingsWebsocketsEnabled = False
          }
    , S.applicationDefinition = definition
    }
    where definition :: Source (ResourceT IO) BL.ByteString -> Sink S.Protocol (ResourceT IO) () -> ResourceT IO ()
          definition source sink = source $= S.mapMessage $$ sink

cookieNeededEcho = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["cookie_needed_echo"]
          , S.settingsCookiesNeeded = True
          }
    , S.applicationDefinition = definition
    }
    where definition :: Source (ResourceT IO) BL.ByteString -> Sink S.Protocol (ResourceT IO) () -> ResourceT IO ()
          definition source sink = source $= S.mapMessage $$ sink

harrEcho = S.Application
    { S.applicationSettings = def
          { S.settingsApplicationPrefix = ["harr_echo"]
          }
    , S.applicationDefinition = definition
    }
    where definition :: Source (ResourceT IO) BL.ByteString -> Sink S.Protocol (ResourceT IO) () -> ResourceT IO ()
          definition source sink = source =$= C.map ("Harr! " <>) =$= S.mapMessage $$ sink

router :: [S.Application m] -> [TS.Text] -> Maybe (S.Application m)
router apps pathInfo = find (\app -> S.settingsApplicationPrefix (S.applicationSettings app) `isPrefixOf` pathInfo) apps

main = do
    sessions <- newMVar HM.empty
    let state =
            S.ServerState
                { S.serverEnvironment =
                      S.ServerEnvironment
                          { S.environmentSessions = sessions
                          }
                , S.serverApplicationRouter = router [echo, harrEcho, disabledWebsocketEcho, cookieNeededEcho, close]
                , S.serverSettings = def
                }
    S.runSockServer 8080 state