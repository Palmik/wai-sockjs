module Network.Wai.Sock.Application
( Application(..)
, ApplicationSettings(..)
) where

------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Data.Conduit         as C  (Source, Sink)    
import qualified Data.Text            as TS (Text)
------------------------------------------------------------------------------

data Application m = Application
    { applicationDefinition :: C.Source m BL.ByteString -> C.Sink BL.ByteString m () -> m ()
    , applicationSettings :: ApplicationSettings
    }

data ApplicationSettings = ApplicationSettings
    { appSettingsPrefix :: [TS.Text]
    }