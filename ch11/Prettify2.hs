module Prettify2 where

import Test.QuickCheck
import Control.Monad (liftM, liftM2)
import Prettify
import Data.List (intersperse)

instance Arbitrary Doc where
    arbitrary =
        oneof [ return Empty
              , liftM  Char   arbitrary
              , liftM  Text   arbitrary
              , return Line
              , liftM2 Concat arbitrary arbitrary
              , liftM2 Union  arbitrary arbitrary ]

prop_empty_id ::  Doc -> Bool
prop_empty_id x = empty <> x == x && x <> empty == x

prop_char ::  Char -> Bool
prop_char c = char c == Char c

prop_text ::  String -> Bool
prop_text s = text s == if null s then Empty else Text s

prop_line ::  Bool
prop_line = line == Line

prop_double ::  Double -> Bool
prop_double d = double d == text (show d)

prop_hcat :: [Doc] -> Bool
prop_hcat xs = hcat xs == foldr (<>) empty xs

prop_punctuate ::  Doc -> [Doc] -> Bool
prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
    where combine [] = []
          combine [x] = [x]
          combine (x:Empty:ys) = x : combine ys
          combine (Empty:y:ys) = y : combine ys
          combine (x:y:ys) = x `Concat` y : combine ys
