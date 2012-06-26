module Control.Concurrent.STM.TMChan.Extra
( getTMChanContents
) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMChan
------------------------------------------------------------------------------

getTMChanContents :: TMChan a -> STM [a]
getTMChanContents ch = do
    mx <- tryReadTMChan ch
    case mx of
         Just (Just x) -> (x:) <$> getTMChanContents ch
         _             -> return []