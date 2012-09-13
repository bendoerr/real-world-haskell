-- Basic IO Example
main = do
       putStrLn "Greetings! What is your name?"
       inpStr <- getLine
       putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
