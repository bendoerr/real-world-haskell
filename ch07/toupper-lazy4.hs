-- Example of Lazy I/O, with stdout
--
import Data.Char(toUpper)

main :: IO ()
main = interact (map toUpper)
