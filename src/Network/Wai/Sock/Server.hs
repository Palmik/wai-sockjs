{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Server
( ServerSettings(..)
, ServerState(..)
, Server(..)

, getServerApplicationRouter
, getServerEnvironment
, getServerSettings
, runServer
) where

------------------------------------------------------------------------------
import           Control.Monad.Trans.State.Lazy
------------------------------------------------------------------------------
import qualified Data.Text    as TS (Text)
import qualified Data.Conduit as C
import           Data.Default
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (ServerSettings(..), ServerState(..), Server(..), Environment(..), Application(..))
------------------------------------------------------------------------------

runServer :: Server a -> ServerState -> C.ResourceT IO a
runServer = evalStateT

getServerSettings :: Server ServerSettings
getServerSettings = gets serverSettings

getServerEnvironment :: Server Environment
getServerEnvironment = gets serverEnvironment

getServerApplicationRouter :: Server ([TS.Text] -> Maybe (Application (C.ResourceT IO)))
getServerApplicationRouter = gets serverApplicationRouter

instance Default ServerSettings where
    def = ServerSettings
              { serverSettingsWebsocketsEnabled = True
              , serverSettingsCookiesNeeded = True
              , serverSettingsAllowedOrigins = "*:*"
              , serverSettingsSockURL = "http://cdn.sockjs.org/sockjs-0.3.min.js"
              , serverSettingsSockVersion = "0.3"
              }