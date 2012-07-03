{-# LANGUAGE FlexibleContexts  #-}

module Network.Sock.Types.Application
( Application(..)
, ApplicationSettings(..)
) where

------------------------------------------------------------------------------
import qualified Data.Conduit         as C (Source, Sink)
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Application related types.

data Application m = Application
    { applicationDefinition :: C.Source m BL.ByteString -> C.Sink BL.ByteString m () -> m ()
    , applicationSettings :: ApplicationSettings
    }

data ApplicationSettings = ApplicationSettings
    { settingsApplicationPrefix :: [TS.Text]
    , settingsCookiesNeeded :: Bool
    , settingsAllowedOrigins :: [TS.Text]
    , settingsWebsocketsEnabled :: Bool
    , settingsSockURL :: TS.Text
    }

