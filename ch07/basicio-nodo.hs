-- Example of using a sequance of steps without a do block
--
main = 
    putStrLn "Greetings! What is your name?" >>
    getLine >>=
    (\input -> putStrLn $ "Welcome to Haskell, " ++ input ++ "!")
