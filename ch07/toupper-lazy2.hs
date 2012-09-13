-- Example of Lazy I/O, more concise
--
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inHandle <- openFile "input.txt" ReadMode
       outHandle <- openFile "output.txt" WriteMode
       inString <- hGetContents inHandle
       let result = (map toUpper inString)
       hPutStr outHandle result
       hClose inHandle
       hClose outHandle

