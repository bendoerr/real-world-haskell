-- Example of Lazy I/O, even more concise
--
import Data.Char(toUpper)

main :: IO ()
main = do
       inString <- readFile "input.txt"
       writeFile "output.txt" (map toUpper inString)
