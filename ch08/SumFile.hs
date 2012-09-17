-- Inefficent processing of a file full of numbers
--
import qualified Data.ByteString.Lazy.Char8 as BS

inefficent = do
    contents <- getContents
    print (sumFile contents)
  where sumFile = sum . map read . words

myInt w = case BS.readInt w of
            Nothing     -> 0
            Just (n,x)  -> n

-- simplify by assuming one number per line/space
efficent = do
    contents <- BS.getContents
    print (sumFile contents)
  where sumFile = sum . map myInt . BS.words

-- runghc SumFile.hs +RTS -s < numbers.txt
main = inefficent
