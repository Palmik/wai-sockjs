module Control.Concurrent.STM.TMChan.Extra
( getTMChanContents
, writeTMChanList
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
------------------------------------------------------------------------------

writeTMChanList :: TMChan a -> [a] -> STM ()
writeTMChanList = mapM_ . writeTMChan

getTMChanContents :: TMChan a -> STM [a]
getTMChanContents ch = do
    mx <- tryReadTMChan ch
    case mx of
         Just (Just x) -> (x:) <$> getTMChanContents ch
         _             -> return []