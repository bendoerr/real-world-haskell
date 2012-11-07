module Barcode.Encode where

import Data.Array ((!))
import Data.Char (digitToInt)

import Barcode

-- | The string to encode is twelve digits long, with encodeDigits adding a
--   thirteenth check digit.  The barcode is encoded as two groups of six
--   digits, with a guard sequence in the middle and “outside” sequences on
--   either side.
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

-- | This function computes the check digit; don't pass one in.
encodeDigits :: [Int] -> [String]
encodeDigits s@(f:r) =
        outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
    where (left, right) = splitAt 6 r
          lefties = zipWith leftEncode (parityCodes ! f) left
          righties = map rightEncode (right ++ [checkDigit s])
encodeDigits _ = error "not supported"

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)
leftEncode _   = error "unknown value"

rightEncode :: Int -> String
rightEncode = (rightCodes !)
