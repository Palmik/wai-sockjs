module Data.List.Utils where

groupWith :: (a -> a -> Maybe a) -> [a] -> [a]
groupWith f (x1:x2:xs) =
    case f x1 x2 of
        Just x  -> groupWith f (x:xs)
        Nothing -> x1 : groupWith f (x2:xs)
groupWith _ xs = xs
