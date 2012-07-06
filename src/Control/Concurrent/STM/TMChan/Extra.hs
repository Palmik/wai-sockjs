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

-- | If the channel is open, always returns list with al least one value.
--   If the channel is closed, returns an empty list.
getTMChanContents :: TMChan a -> STM [a]
getTMChanContents ch = go 0
    where go n = do
              mx <- tryReadTMChan ch
              case mx of
                   Just (Just x) -> (x:)  <$> go (n + 1) -- There was a value in the channel.
                   Just Nothing  -> if (n > 0)
                                       then return []               -- The channel was empty but we have acummulated some data already, so we return what we've got.
                                       else maybe [] (:[]) <$> readTMChan ch -- The channel was empty and we have not recovered any value yet, so we wait.
                   _             -> return [] -- The channel was closed.