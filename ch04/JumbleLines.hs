import System.Environment (getArgs)

isNewLine c | c == '\n' = True
            | otherwise = False

joinEm c1 c2 = c1 : ( c2 : [] )

jumble s = unlines (zipWith joinEm front tailBack)
    where (front, back) = break isNewLine s
          tailBack = tail back
          
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = jumble

