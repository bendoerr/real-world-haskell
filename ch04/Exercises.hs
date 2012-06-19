import Data.Char (digitToInt, isDigit)

-- Write your own “safe” definitions of the standard partial list functions,
-- but make sure they never fail.
--
-- These ended up being just wrappers.
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail xs = Just (tail xs)

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast xs = Just (last xs)

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)

-- Write a function splitWith that acts similarly to words but takes a
-- predicate and a list of any type, and then splits its input list on every
-- element for which the predicate returns False
splitWith _ [] = []
splitWith p xs | null front = splitWith p backTail
               | null back  = [front]
               | otherwise  = front : splitWith p backTail
          where (front, back) = span p xs
                backTail      = tail back

-- Using the command framework from the earlier section “A Simple Command-Line
-- Framework” on page 71, write a program that prints the first word of each
-- line of its input.
l = "abc foo\ndef bar\nhij baz\nklm boo"

firstWords s = unlines (map each (lines s))
    where each [] = []
          each s = head (words s)


-- Write a program that transposes the text in a file. For instance, it should
-- convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".
isNewLine c | c == '\n' = True
            | otherwise = False

joinEm c1 c2 = c1 : ( c2 : [] )

jumble s = unlines (zipWith joinEm front tailBack)
    where (front, back) = break isNewLine s
          tailBack = tail back
         
-- Use a fold (choosing the appropriate fold will make your code much simpler)
-- to rewrite and improve upon the asInt function from the earlier
-- section “Explicit Recursion” on page 85.
asInt_fold :: String -> Int
asInt_fold ""                   = 0
asInt_fold (x:xs) | x == '-'    = (-1) * asInt_fold xs
                  | otherwise   = foldl charAsInt 0 (x:xs)
  where charAsInt :: Int -> Char -> Int
        charAsInt acc x = acc * 10 + digitToInt x 

asInt_fold_error :: String -> Int
asInt_fold_error []       = error "Contains no digits"
asInt_fold_error ('-':xs) = -(asInt_fold_error xs)
asInt_fold_error xs       = foldl charAsInt 0 xs
  where charAsInt :: Int -> Char -> Int
        charAsInt acc x | isDigit x = acc * 10 + digitToInt x
                        | otherwise = error ("Not a digit '" ++ [x] ++ "'")

-- #4 is super confusing

-- Write your own definition of concat using foldr.
concat_foldr :: [[a]] -> [a]
concat_foldr xs = foldr (++) [] xs

-- Write your own definition of the standard takeWhile function, first using
-- explicit recursion, and then foldr. 
takeWhile_recursion :: (a -> Bool) -> [a] -> [a]
takeWhile_recursion _ []     = []
takeWhile_recursion f (x:xs) | f x       = x : takeWhile_recursion f xs
                             | otherwise = []

takeWhile_foldr :: (a -> Bool) -> [a] -> [a]
takeWhile_foldr f xs = foldr takeIt [] xs
  where takeIt x acc | f x       = x : acc
                     | otherwise = []

-- Use ghci to load the Data.List module and figure out what groupBy does, then
-- write your own implementation using a fold.

-- This is NOT correct! Works in some cases but not all.
groupBy_foldr :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldr f xs = foldr groupIt [[]] xs
    where groupIt y [[]]                 = [[y]]
          groupIt y (x:acc) | f y (head x) = (y:x) : acc
                            | otherwise   = [y] : (x:acc)

-- This is correct, but could use some cleaning. The lasts aren't great.
groupBy_foldl :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy_foldl f (x:xs) = foldl groupIt [[x]] xs
  where groupIt acc y | f (head (last acc)) y = (init acc) ++ [(last acc) ++ [y]]
                      | otherwise = acc ++ [[y]]

-- How many of the following Prelude functions can you rewrite using list folds?
--  any
--  cycle
--  words
--  unlines

-- Any could be written using foldl or foldr. Short circut of OR.
any_foldr pred xs = foldr (||) False (map pred xs) -- Lazy evaluation here too (I think) with the map.
                      
-- Cycle???

-- Words can use foldl or foldr.
-- foldr lets us use head/tail rather than last/init.
words_foldr :: [Char] -> [[Char]] 
words_foldr str = let words = foldr gWords [[]] str
                  in if null (head words) -- Maybe there is a better way to handle the leading space?
                     then tail words
                     else words
  where gWords :: Char -> [[Char]] -> [[Char]]
        gWords ' ' [[]]   = [[]] -- Trailing space
        gWords c   [[]]   = [[c]] -- First letter
        gWords ' ' (w:ws) | null w    = w:ws -- Multiple spaces
                          | otherwise = [] : (w:ws) -- Single space
        gWords c   (w:ws) = (c:w):ws -- Add to word.

unlines_foldr lines = foldr join "" lines
  where join l str = l ++ "\n" ++ str

-- same just using annyonmous function
unlines_foldr2 lines = foldr (\l s -> l ++ "\n" ++ s) "" lines
