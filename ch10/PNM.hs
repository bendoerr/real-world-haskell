module PNM (
    -- * Data Type
      Greymap (..)
    -- * Parsing
    , parseP5t1, parseP5t2
    -- * Parsing Helpers
    , matchHeader, getNaturalNumber, getBytes
) where

import qualified Data.ByteString.Lazy.Char8 as BS8
import qualified Data.ByteString.Lazy as BS

import Data.ByteString.Lazy (ByteString)
import Data.Char (isSpace)

-- | Represent the header and data of the PGM P5 as it's own data type. Record
--   syntatic sugar for easy getters.
data Greymap = Greymap { greyWidth  :: Int        -- ^ The width of the PGM.
                       , greyHeight :: Int        -- ^ The hight of the PGM.
                       , grayMax    :: Int        -- ^ Maximum grey value.
                       , grayData   :: ByteString -- ^ Binary data of image.
                       } deriving (Eq)

-- Avoid having show print the data as well since this could be overwhelming.
instance Show Greymap where
    show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++
                             "  " ++ show m

-- | A very literal parse implementation that is one big staircase of case
--   expressions with each function returing the residual bytestring. This can
--   be improved.
parseP5t1 :: ByteString -> Maybe (Greymap, ByteString)
parseP5t1 s =
    case matchHeader (BS8.pack "P5") s of
        Nothing -> Nothing
        Just s1 ->
            case getNaturalNumber s1 of
                Nothing -> Nothing
                Just (width, s2) ->
                    case getNaturalNumber (BS8.dropWhile isSpace s2) of
                        Nothing -> Nothing
                        Just (height, s3) ->
                            case getNaturalNumber (BS8.dropWhile isSpace s3) of
                                Nothing -> Nothing
                                Just (maxGrey, s4)
                                    | maxGrey > 255 -> Nothing
                                    | otherwise ->
                                        case getBytes 1 s4 of
                                            Nothing -> Nothing
                                            Just (_, s5) ->
                                                case getBytes (width * height) s5 of
                                                    Nothing -> Nothing
                                                    Just (bitmap, s6) ->
                                                        Just (Greymap width height maxGrey bitmap, s6)

-- | Checks if a prefix is present on a dataset and returns the remaning data
--   after the prefix and any spaces if it is found.
matchHeader :: ByteString       -- ^ Prefix to check for.
            -> ByteString       -- ^ The context to check.
            -> Maybe ByteString -- ^ Nothing if the prefix isn't found, and the
                                --   remaning content after prefix if found.
matchHeader prefix str
    | prefix `BS8.isPrefixOf` str
        = Just $ BS8.dropWhile isSpace $ BS.drop (BS.length prefix) str
    | otherwise = Nothing

-- | Reads a number from the begining of the string. If there is no number at
--   the beginning of the string or the number is less than or equal to 0 then
--   it returns Nothing otherwise it returns the number and the rest of the
--   string.
getNaturalNumber :: ByteString
                 -> Maybe (Int, ByteString)
getNaturalNumber s = case BS8.readInt s of
                    Nothing -> Nothing
                    Just (num, remaining)
                        | num <= 0  -> Nothing
                        | otherwise -> Just (fromIntegral num, remaining)

-- | Reads n number of bytes from a byte string returning those bytes and the
--   remaining bytes.
getBytes :: Int        -- ^ Number of bytes to read.
         -> ByteString -- ^ The content to read from.
         -> Maybe (ByteString, ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix, _) = BS.splitAt count str
                 in if BS.length prefix < count
                    then Nothing
                    else Just both

-- As I understand it this is basically the equivalent of the >>= for Monads.
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v  >>? f = f v

-- | Implementation of our parse function using our new >>? function.
parseP5t2 :: ByteString -> Maybe (Greymap, ByteString)
parseP5t2 s = matchHeader (BS8.pack "P5") s
          >>? \s1 -> skipSpace ((), s1)
          >>? (getNaturalNumber . snd)
          >>? skipSpace
          >>? \(width, s2) -> getNaturalNumber s2
          >>? skipSpace
          >>? \(height, s3) -> getNaturalNumber s3
          >>? \(maxGrey, s4) -> getBytes 1 s4
          >>? (getBytes (width * height) . snd)
          >>? \(bm, s5) -> Just (Greymap width height maxGrey bm, s5)
    where skipSpace :: (a, ByteString) -> Maybe (a, ByteString)
          skipSpace (a, ss) = Just (a, BS8.dropWhile isSpace ss)
