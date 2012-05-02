{-# LANGUAGE GADTs #-}

module Network.Sock.Types
( ClientFrame(..)
, ServerOptions(..)
) where

------------------------------------------------------------------------------
--import           Data.Aeson           (ToJSON)
import           Data.Default
import           Data.Int             (Int64)
import           Data.Text.Lazy       (Text)
import           Data.ByteString.Lazy (ByteString)
------------------------------------------------------------------------------

data ClientFrame where
    FrameOpen      :: ClientFrame
    FrameHeartbeat :: ClientFrame
    FrameMessage   :: ByteString -> ClientFrame
    FrameClose     :: Int64 -> Text -> ClientFrame

data ServerOptions = ServerOptions
    { soWebsocketsEnabled :: Bool
    , soCookiesNeeded     :: Bool
    }

instance Default ServerOptions where
    def = ServerOptions
              { soWebsocketsEnabled = True
              , soCookiesNeeded = True
              }