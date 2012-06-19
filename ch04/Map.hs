import Data.Char (toUpper)

-- void square(double *out, const double *in, size_t length) {
--     for(size_t i = 0; i < length; i++) {
--         out[i] = in[i] * in[i];
--     }
-- }

square :: [Double] -> [Double]
square (x:xs) = x * x : square xs
square []     = []  

-- #include <ctype.h>
--
-- char *uppercase(const char *in) {
--     char *out = strdup(in);
--     if(out != NULL) {
--         for (size_t i = 0; out[i] != '\0'; i++) {
--             out[i] = toupper(out[i]); 
--         }
--     }
--     return out;
-- }

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
upperCase []     = []


-- Using Haskell's Map function
square2 xs = map squareIt xs
    where squareIt x = x * x

upperCase2 xs = map toUpper xs


myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []
