module Barcode.Decode (
    -- * Greyscaling
      Greymap, Bit(..), pixmapToGreymap
    -- * Processing
    , luminance, threshold, runLengths, rlEncode, asSRL, distance, bestScores
    , fromParity, parityMap, compareWithoutParity, bestLeft, bestRight
    -- * Scaled Run length encoded codes
    , leftOddSRL, leftEvenSRL, rightSRL, paritySRL
    -- * Generating candidate digits
    , candidateDigits, useDigit, incorporateDigits, finalDigits, buildDigitMap, solve
) where

import qualified Data.Map as M

import Control.Applicative ((<$>))
import Data.Array (Array)
import Data.Function (on)
import Data.List (group, sort, sortBy, foldl')
import Data.Ix (Ix)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Ratio (Ratio, (%))
import Data.Word (Word8)

import Arrays (foldA1)
import Barcode (leftOddList, leftEvenList, rightList, parityList)
import Lists (chunksOf)
import PNM (Pixel, Pixmap)

type Greymap = Array (Int, Int) Pixel

data Bit = Zero | One
           deriving (Eq, Show)

type Run = Int
type RunLength a = [(Run, a)]

type Score = Ratio Int
type ScoreTable = [[Score]]

type Digit = Word8

data Parity a = Even a | Odd a | None a
                deriving (Show)

instance Functor Parity where fmap = parityMap

type CheckMap a = Map Digit [a]
type DigitMap = CheckMap Digit
type ParityMap = CheckMap (Parity Digit)



-- | Covert color pixel to a greyscale pixel based on the perceived brightness
--   of each color channel.
luminance :: (Pixel, Pixel, Pixel) -> Pixel
luminance (r, g, b) = round (rn * rLum + gn * gLum + bn * bLum)
    where rn = fromIntegral r
          gn = fromIntegral g
          bn = fromIntegral b
          rLum = 0.30 :: Double
          gLum = 0.59 :: Double
          bLum = 0.11 :: Double

-- | Coverts an entire pixmap to greyscale using it's luminance.
pixmapToGreymap :: Pixmap -> Greymap
pixmapToGreymap = fmap luminance

-- | Computes the minimum and maximum values in its input array. It takes these
--   and a threshold valued between zero and one, and computes a “pivot” value.
--   Then for each value in the array, if that value is less than the pivot,
--   the result is Zero, otherwise One.
threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot = round $ least + (great - least) * n
          least = fromIntegral $ choose (<) a
          great = fromIntegral $ choose (>) a
          choose f = foldA1 $ \x y -> if f x y then x else y

rlEncode :: Eq a => [a] -> RunLength a
rlEncode = map rle . group
    where rle xs = (length xs, head xs)

runLengths :: Eq a => [a] -> [Run]
runLengths = map fst . rlEncode

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map (% sum xs) xs

asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne . runLengths)

leftOddSRL, leftEvenSRL, rightSRL, paritySRL ::  ScoreTable
leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL = asSRL rightList
paritySRL = asSRL parityList


distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

bestScores :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScores srl ps = take 3 . sort $ scores
    where scores = zip [distance d (scaleToOne ps) | d <- srl] digits
          digits = [0..9]

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a -> b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

compareWithoutParity :: (Ord a) => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity

bestLeft :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
               (map Odd  (bestScores leftOddSRL  ps)) ++
                map Even (bestScores leftEvenSRL ps)

bestRight :: [Run] -> [Parity (Score, Digit)]
bestRight = map None . bestScores rightSRL

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One) : _) = []
candidateDigits rle | length rle < 59 = []
                    | any null match  = []
                    | otherwise       = map (map (fmap snd)) match
    where match = map bestLeft left ++ map bestRight right
          left = chunksOf 4 . take 24 . drop 3 $ rls
          right = chunksOf 4 . take 24 . drop 32 $ rls
          rls = map fst rle

updateMap :: Parity Digit       -- ^ new digit
          -> Digit              -- ^ existing key
          -> [Parity Digit]     -- ^ existing digit sequence
          -> ParityMap          -- ^ map to update
          -> ParityMap
updateMap digit key dseq = insertMap key (fromParity digit) (digit:dseq)

insertMap :: Digit -> Digit -> [a] -> CheckMap a -> CheckMap a
insertMap key digit val m = val `seq` M.insert key' val m
    where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
    new `M.union` M.foldWithKey (updateMap digit) M.empty old

incorporateDigits :: ParityMap -> [Parity Digit] -> ParityMap
incorporateDigits old = foldl' (useDigit old) M.empty

finalDigits :: [[Parity Digit]] -> ParityMap
finalDigits = foldl' incorporateDigits (M.singleton 0 []) 
            . mapEveryOther (map (fmap (*3)))
    where mapEveryOther f = zipWith ($) (cycle [f,id])

firstDigit :: [Parity a] -> Digit
firstDigit = snd
           . head
           . bestScores paritySRL
           . runLengths
           . map parityBit
           . take 6
  where parityBit (Even _) = Zero
        parityBit (Odd _) = One

addFirstDigit :: ParityMap -> DigitMap
addFirstDigit = M.foldWithKey updateFirst M.empty

updateFirst :: Digit -> [Parity Digit] -> DigitMap -> DigitMap
updateFirst key seq = insertMap key digit (digit:renormalize qes)
  where renormalize = mapEveryOther (`div` 3) . map fromParity
        digit = firstDigit qes
        qes = reverse seq
        mapEveryOther f = zipWith ($) (cycle [f,id])

buildDigitMap :: [[Parity Digit]] -> DigitMap
buildDigitMap = M.mapKeys (10 -)
         . addFirstDigit
         . finalDigits

solve :: [[Parity Digit]] -> [[Digit]]
solve [] = []
solve xs = catMaybes $ map (addCheckDigit m) checkDigits
    where checkDigits = map fromParity (last xs)
          m = buildDigitMap (init xs)
          addCheckDigit m k = (++[k]) <$> M.lookup k m
