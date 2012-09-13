-- Example of Lazy I/O
--
import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
       inHandle <- openFile "input.txt" ReadMode
       outHandle <- openFile "output.txt" WriteMode
       inString <- hGetContents inHandle
       let result = processData inString
       hPutStr outHandle result
       hClose inHandle
       hClose outHandle

processData :: String -> String
processData = map toUpper
