module Control.Monad.Trans.Resource.Extra
( ReleaseKeyF
, allocateF
, registerF
, releaseF
) where

------------------------------------------------------------------------------
import qualified Control.Monad.Trans.Resource as R
import           Control.Monad.IO.Class            (liftIO)
------------------------------------------------------------------------------
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
------------------------------------------------------------------------------

data ReleaseKeyF a = ReleaseKeyF (IORef a) R.ReleaseKey

allocateF :: R.MonadResource m
          => IO a
          -> (b -> a -> IO ())
          -> b
          -> m (ReleaseKeyF b, a)
allocateF alloc free initial = do
    ref <- liftIO $ newIORef initial
    let free' a = do
            b <- readIORef ref
            free b a
    (key, a) <- R.allocate alloc free'
    return (ReleaseKeyF ref key, a)

registerF :: R.MonadResource m
          => (a -> IO ())
          -> a
          -> m (ReleaseKeyF a)
registerF f a0 = do
    ref <- liftIO $ newIORef a0
    key <- R.register $ readIORef ref >>= f
    return $! ReleaseKeyF ref key

releaseF :: R.MonadResource m => (ReleaseKeyF a) -> a -> m ()
releaseF (ReleaseKeyF ref key) a = do
    liftIO $ writeIORef ref a
    R.release key
    