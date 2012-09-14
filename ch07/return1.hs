-- Avoiding type errors with the IO Monad using return.
--
import Data.Char(toUpper)

isGreen :: IO Bool
isGreen =
    do putStrLn "Is green your favorite color?"
       inpStr <- getLine
       return ((toUpper . head $ inpStr) == 'Y')

--------------
-- return2.hs
-- pure function broken out
--
isYes2 :: String -> Bool
isYes2 s = (toUpper . head $ s) == 'Y'

isGreen2 :: IO Bool
isGreen2 =
    putStrLn "Is green your favorite color?" >>
    getLine >>=
    (\inS -> return (isYes2 inS))

-------------
-- return3.hs
-- contrived example showing return doesn't need to be last
--
returnTest3 :: IO ()
returnTest3 =
    do one <- return 1
       let two = 2
       putStrLn $ show (one + two)

