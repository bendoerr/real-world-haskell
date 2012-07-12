main = do
  putStrLn "Please enter a double:"
  inpStr <- getLine
  let inpDouble = (read inpStr)::Double
  putStrLn ("Twice" ++ show inpDouble ++ " is " ++ show (inpDouble * 2))
