-- | Various Array specific Helpers for this chapter.
module Arrays where

import Data.Array (Array, elems, listArray)
import Data.List (foldl')
import Data.Ix (Ix(..))

-- | Helper to create an Array out of a List
listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, bound) xs
    where bound = length xs - 1


-- | Strict left fold, similar to foldl' on lists.
foldA :: Ix k => (a -> b -> a) -> a -> Array k b -> a
foldA f s = foldl' f s . elems

-- | Strict left fold using the first element of the array as its starting
--   value, similar to foldl1 on lists.
foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f = foldl1 f . elems


