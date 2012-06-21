{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Sock.Server
( ServerSettings(..)
) where

------------------------------------------------------------------------------
import qualified Data.Text    as TS (Text)
import           Data.Default
------------------------------------------------------------------------------

data ServerSettings = ServerSettings
    { serverSettingsWebsocketsEnabled :: Bool
    , serverSettingsCookiesNeeded :: Bool
    , serverSettingsAllowedOrigins :: TS.Text
    , serverSettingsSockURL :: TS.Text
    , serverSettingsSockVersion :: TS.Text
    }

instance Default ServerSettings where
    def = ServerSettings
              { serverSettingsWebsocketsEnabled = True
              , serverSettingsCookiesNeeded = True
              , serverSettingsAllowedOrigins = "*:*"
              , serverSettingsSockURL = "http://cdn.sockjs.org/sockjs-0.3.min.js"
              , serverSettingsSockVersion = "0.3"
              }