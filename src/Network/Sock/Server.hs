{-# LANGUAGE OverloadedStrings #-}

module Network.Sock.Server
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
import           Network.Sock.Types.Application
import           Network.Sock.Types.Server
------------------------------------------------------------------------------

runServer :: Server a -> ServerState -> C.ResourceT IO a
runServer = evalStateT

getServerSettings :: Server ServerSettings
getServerSettings = gets serverSettings

getServerEnvironment :: Server ServerEnvironment
getServerEnvironment = gets serverEnvironment

getServerApplicationRouter :: Server ([TS.Text] -> Maybe (Application (C.ResourceT IO)))
getServerApplicationRouter = gets serverApplicationRouter

instance Default ServerSettings where
    def = ServerSettings
              { settingsSockVersion = "0.3"
              }