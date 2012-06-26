module Control.Concurrent.MVar.Extra
( mvar
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar
------------------------------------------------------------------------------

-- | The MVar "catamorphism"
mvar :: IO b        -- ^ The result when the MVar is empty.
     -> (a -> IO (a, b)) -- ^ Function to apply when the the MVar is not empty.
     -> MVar a
     -> IO b
mvar action f m = do
    mv <- tryTakeMVar m
    case mv of
         Nothing -> action     -- ^ The MVar was empty. We run the supplied action.
         Just x  -> do         -- ^ The MVar contained x.
             (x', y) <- f x    -- ^ We run the supplied function on x.
             putMVar m x'      -- ^ Put the newly computed x' into the MVar.
             return y          -- ^ Return the computed y