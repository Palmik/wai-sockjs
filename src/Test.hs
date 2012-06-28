{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.MVar.Lifted
import qualified Data.HashMap.Lazy as HM
import           Data.Conduit
import           Data.Default
import           Data.List
import qualified Data.Text as TS
import           Network.Wai.Sock
import qualified Network.Wai.Handler.Warp as W

echo = Application
    { applicationSettings = ApplicationSettings ["echo"]
    , applicationDefinition = def
    }
    where def source sink = source $$ sink

router :: [Application m] -> [TS.Text] -> Maybe (Application m)
router apps pathInfo = find (\app -> applicationPrefix (applicationSettings app) `isPrefixOf` pathInfo) apps

runSockServer :: W.Port -> ([TS.Text] -> Maybe (Application (ResourceT IO))) -> IO ()
runSockServer p r = do
    mvsm <- newMVar HM.empty
    let env = Environment { envSessions = mvsm }
    W.runSettings W.defaultSettings { W.settingsPort = p } (sock def env r)

main = runSockServer 8080 (router [echo])