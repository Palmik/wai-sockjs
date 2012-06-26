{-# LANGUAGE FlexibleContexts #-}

module Control.Concurrent.MVar.Extra.Lifted
( mvar
) where

------------------------------------------------------------------------------
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Base
------------------------------------------------------------------------------

-- | The MVar "catamorphism"
mvar :: MonadBase IO m
     => m b             -- ^ The result when the MVar is empty.
     -> (a -> m (a, b)) -- ^ Function to apply when the the MVar is not empty.
     -> MVar a
     -> m b
mvar action f m = do
    mv <- tryTakeMVar m
    case mv of
         Nothing -> action     -- ^ The MVar was empty. We run the supplied action.
         Just x  -> do         -- ^ The MVar contained x.
             (x', y) <- f x    -- ^ We run the supplied function on x.
             putMVar m x'      -- ^ Put the newly computed x' into the MVar.
             return y          -- ^ Return the computed y