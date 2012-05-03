{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Application.Sock.Types
( ResponseInfo(..)
) where

------------------------------------------------------------------------------
import           Data.Aeson
import           Data.Int             (Int64)
import           Data.Conduit
import           Data.ByteString.Lazy (ByteString)
------------------------------------------------------------------------------
import           Network.Sock.Types
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- | Used on "/info"
data ResponseInfo = ResponseInfo
    { riWebsocketsEnabled :: Bool
    , riCookiesNeeded :: Bool
    , riOrigins :: String
    , riEntropy :: Int64
    }

instance ToJSON ResponseInfo where
    toJSON ResponseInfo{..} = object
        [ "websocket"     .= riWebsocketsEnabled
        , "cookie_needed" .= riCookiesNeeded
        , "origins"       .= riOrigins
        , "entropy"       .= riEntropy
        ]