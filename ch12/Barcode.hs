-- | Library for working with EAN-13 barcodes.
--
--   Structure of an EAN-13 barcode:
--
--   * The first two digits describe the number system. This can either
--     indicate the nationality of the manufacturer, or describe one of a few
--     other categories, such as ISBN (book identifier) numbers.
--
--   * The next five digits are a manufacturer ID, assigned by a country's
--     numbering authority.
--
--   * The five digits that follow are a product ID, assigned by the
--     manufacturer. (Smaller manufacturers may have a longer manufacturer ID
--     and shorter product ID, but they still add up to ten digits.)
--
--   * The last digit is a check digit, allowing a scanner to validate the
--     digit string it scans.
--
module Barcode (
    -- * Barcode Structure
      checkDigit
    -- Codes
    , leftOddList, leftOddCodes, leftEvenList, leftEvenCodes
    , rightList, rightCodes, parityList, parityCodes
    , outerGuard, centerGuard
) where

import Control.Applicative ((<$>))
import Data.Array (Array)

import Arrays

-- | Calculates the check digit of a numeral represenation of a barcode.
--
--   Step 1: Multiply the sum of the odd position digits by 3
--   Step 2: Sum the even position digits,
--   Step 3: Sum the results from step 1 and step 2,
--   Step 4: The check digit is 10 minus the result of step 3 modulo 10.
checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum (mapEveryOther (* 3) (reverse ds)) `mod` 10)
    where mapEveryOther f = zipWith ($) (cycle [f,id])

-- | Various code representations.
--   Each digit in the left group is encoded using either odd or even parity,
--   with the parity chosen based on the bits of the first digit in the string.
--   If a bit of the first digit is zero, the corresponding digit in the left
--   group is encoded with even parity. A one bit causes the digit to be
--   encoded with odd parity. This encoding is an elegant hack, chosen to make
--   EAN-13 barcodes backwards compatible with the older UPC-A standard.
leftOddList, leftEvenList, rightList, parityList :: [String]
leftOddCodes, leftEvenCodes, rightCodes, parityCodes :: Array Int String

leftOddCodes = listToArray leftOddList
leftOddList = ["0001101", "0011001", "0010011", "0111101", "0100011",
               "0110001", "0101111", "0111011", "0110111", "0001011"]

leftEvenCodes = listToArray leftEvenList
leftEvenList = map reverse rightList

rightCodes = listToArray rightList
rightList = map complement <$> leftOddList
    where complement '0' = '1'
          complement '1' = '0'
          complement _   = error "unknown value"

parityCodes = listToArray parityList
parityList = ["111111", "110100", "110010", "110001", "101100",
              "100110", "100011", "101010", "101001", "100101"]

outerGuard :: String
outerGuard = "101"

centerGuard :: String
centerGuard = "01010"
