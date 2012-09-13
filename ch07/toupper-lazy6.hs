-- Example of Lazy I/O, interacting with stdout
--
import Data.Char(toUpper)

main = interact ((++) "You data, in uppercase, is:\n\n" . map toUpper)
