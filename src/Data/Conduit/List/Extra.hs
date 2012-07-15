module Data.Conduit.List.Extra
( isolateWhile
, ignoreWhile
, takeWhile
, dropWhile
) where

import           Prelude           hiding (takeWhile, dropWhile)
import qualified Data.Conduit      as C
import qualified Data.Conduit.List as C

-- | Constructs list from upstream values while the predicate holds (and while there are any values left).
-- If you want to pipe the values instead of accumulating them, use isolateWhile.
-- This function is semantically equivalent to:
--
-- > isolateWhile pr =$ consume
-- 
takeWhile :: Monad m => (a -> Bool) -> C.Sink a m [a]
takeWhile pr = isolateWhile pr C.=$ C.consume

-- | Constructs list from upstream values, discarding them while the predicate holds (and while there are any values left).
-- This function is semantically equivalent to:
--
-- > takeWhile pr >> return ()
--
-- This function is consistent with `Data.Conduit.List.drop`, if you want it
-- to be consistent with `Data.List.drop` instead use:
--
-- > dropWhile pr >> consume
--
-- Or alternatively:
--
-- > ignoreWhile pr =$ consume
--
dropWhile :: Monad m => (a -> Bool) -> C.Sink a m ()
dropWhile pr = loop
    where loop = C.await >>= maybe (return ())
                                   (\val -> if pr val
                                               then loop
                                               else C.leftover val
                                   )
                                   
-- | Ignores the values from upstream while the predicate holds, after that pipes all the values. Complement to `isolateWhile`.
-- Example:
--
-- >>> sourceList [1..10] $= ignoreWhile (< 3) $$ consume
-- [3,4,5,6,7,8,9,10]
ignoreWhile :: Monad m => (a -> Bool) -> C.Conduit a m a
ignoreWhile pr = loop
    where loop = C.await >>= maybe (return ())
                                   (\val -> if pr val
                                               then loop
                                               else C.leftover val >> C.awaitForever C.yield
                                    )
                                   
-- | Pipes all the the values from upstream while the predicate holds. Complement to `ignoreWhile`.
-- Example:
--
-- >>> sourceList [1..10] $= isolateWhile (< 3) $$ consume
-- [1, 2]
isolateWhile :: Monad m => (a -> Bool) -> C.Conduit a m a
isolateWhile pr = loop
    where loop = C.await >>= maybe (return ())
                                   (\val -> if pr val
                                               then C.yield    val >> loop
                                               else C.leftover val
                                   )