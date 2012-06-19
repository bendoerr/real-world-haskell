-- int as_int(char *str) {
--     int acc;
--
--     for (acc = 0; isdigit(*str); i++) {
--         acc = acc * 10 (*str - '0');
--     }
--
--     return acc;
-- }

import Data.Char (digitToInt)

loop :: Int -> String -> Int
loop acc []     = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs

asInt :: String -> Int
asInt xs = loop 0 xs


