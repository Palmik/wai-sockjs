module Control.Monad.Utils where

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb m1 m2 = do
    b <- mb
    if b then m1 else m2

