{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Control.Applicative
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List
import           Network.Wai.Sock

echo :: Application
echo = Application $ forever (pull >>= push)

router :: [([Text], Application)] -> [Text] -> Maybe (Application, [Text], [Text])
router apps pathInfo = tr <$> find (\(prefix, _) -> prefix `isPrefixOf` pathInfo) apps
    where tr (p, a) = (a, p, drop (length p) pathInfo)

main = runSockServer 8080 (router [(["echo"], echo)])