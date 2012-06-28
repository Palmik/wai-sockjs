{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Server
( ServerSettings(..)
, ServerState(..)
, Server(..)
) where

------------------------------------------------------------------------------
import qualified Data.Text    as TS (Text)
import           Data.Default
------------------------------------------------------------------------------
import           Network.Wai.Sock.Internal.Types (ServerSettings(..), ServerState(..), Server(..))
------------------------------------------------------------------------------

instance Default ServerSettings where
    def = ServerSettings
              { serverSettingsWebsocketsEnabled = True
              , serverSettingsCookiesNeeded = True
              , serverSettingsAllowedOrigins = "*:*"
              , serverSettingsSockURL = "http://cdn.sockjs.org/sockjs-0.3.min.js"
              , serverSettingsSockVersion = "0.3"
              }