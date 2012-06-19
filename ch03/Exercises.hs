import Data.List
import Data.Ord (comparing)

-- Write a function that computes the number of elements in a list. To test
-- it, ensure that it gives the same answers as the standard length function. 
len :: [a] -> Int
len [] = 0
len xs = count 1 (tail xs)
    where count n [] = n
          count n ys = count (n + 1) (tail ys)

-- Write a function that computes the mean of a list, i.e., the sum of all
-- elements in the list divided by its length. (You may need to use the
-- fromIntegral function to convert the length of the list from an integer into
-- a floating-point number.)
mean :: Num a => [a] -> Fractional a => a 
mean [] = 0
mean xs = (sum xs) / fromIntegral(len xs)

-- Turn a list into a palindrome; i.e., it should read the same both backward
-- and forward. For example, given the list [1,2,3], your function should
-- return [1,2,3,3,2,1].
palindrome :: [a] -> [a]
palindrome [] = []
palindrome xs = xs ++ reverse xs

-- Not repeating the middle entry such that [1,2,3] becomes [1,2,3,2,1]
palindrome1 :: [a] -> [a]
palindrome1 [] = []
palindrome1 xs = xs ++ tail(reverse xs)

-- Write a function that determines whether its input list is a palindrome.
-- Simple case only right now with middle entry repeating.
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs | (mod (length xs) 2) == 0 = 
        front == revBack
    where split = splitAt (round((fromIntegral(length xs)) / 2)) xs
          front = fst split
          revBack = reverse (snd split) 
isPalindrome _ = False

-- Create a function that sorts a list of lists based on the length of each
-- sublist. (You may want to look at the sortBy function from the Data.List
-- module.)
sortLists xys = sortBy sizeCompare xys
    where sizeCompare xs ys | xsl == ysl = EQ
                            | xsl > ysl  = GT
                            | otherwise  = LT
            where xsl = length xs
                  ysl = length ys

-- Define a function that joins a list of lists together using a separator
-- value. intersperse :: a -> [[a]] -> [a]
-- The separator should appear between elements of the list, but it should not
-- follow the last element.
intersperse1 :: a -> [[a]] -> [a]
intersperse1 s xs
    | null xs         = []
    | length(xs) == 1 = head xs
    | otherwise       = head xs ++ [s] ++ (intersperse1 s (tail xs))

-- Exercise 8
-- Using the binary tree type that we defined earlier in this chapter, write a
-- function that will determine the height of the tree. The height is the
-- largest number of hops from the root to an Empty. For example, the tree
-- Empty has height zero; Node "x" Empty Empty has height one; Node "x" Empty
-- (Node "y" Empty Empty) has height two; and so on.
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

deepest Empty = 0
deepest (Node _ Empty Empty) = 1
deepest (Node _ left  Empty) = 1 + (deepest left)
deepest (Node _ Empty right) = 1 + (deepest right)
deepest (Node _ left  right) | ldepth > rdepth  = ldepth + 1
                             | otherwise        = rdepth + 1
        where ldepth = deepest left
              rdepth = deepest right

-- cleaner version?
clDeepest t = depth t
    where depth Empty                                  = 0
          depth (Node _ Empty Empty)                   = 1
          depth (Node _ left  Empty)                   = 1 + clDeepest left
          depth (Node _ Empty right)                   = 1 + clDeepest right
          depth (Node _ left  right) | ldepth > rdepth = 1 + ldepth
                                     | otherwise       = 1 + rdepth
                where ldepth = clDeepest left
                      rdepth = clDeepest right


-- cleanest :)
depth (Node _ left right) = 1 + max (depth left) (depth right)
depth Empty               = 0

-- Exercise 9
-- Consider three two-dimensional points, a, b, and c. If we look at the angle
-- formed by the line segment from a to b and the line segment from b to c, it
-- turns left, turns right, or forms a straight line. Define a Direction data
-- type that lets you represent these possibilities.
data Direction = LEFT | RIGHT | STRAIGHT
                 deriving (Show, Eq)

-- Exercise 10
-- Write a function that calculates the turn made by three two-dimensional
-- points and returns a Direction.
data CartesianPoint = Point {x :: Double, y :: Double}
                      deriving (Show)

directionOf a v b | sign > 0  = RIGHT
                  | sign < 0  = LEFT
                  | otherwise = STRAIGHT
            where sign = (x v - x a) * (y b - y a) - (y v - y a) * (x b - x a)
            -- Cross Product from http://en.wikipedia.org/wiki/Graham_scan

-- Exercise 11
-- Define a function that takes a list of two-dimensional points and computes
-- the direction of each successive triple. Given a list of points [a,b,c,d,e],
-- it should begin by computing the turn made by [a,b,c], then the turn made by
-- [b,c,d], then [c,d,e]. Your function should return a list of Direction.
directionsOf (a : v : b : ps) = directionOf a v b : directionsOf(v:b:ps)
directionsOf _                = []

-- Exercise 12
-- Using the code from the preceding three exercises, implement Graham’s scan
-- algorithm for the convex hull of a set of 2D points. You can find good
-- description of what a convex hull (http://en.wikipedia.org/wiki/Convex_hull)
-- is, and how the Graham scan algorithm
-- (http://en.wikipedia.org/wiki/Graham_scan) should work, on Wikipedia
-- (http://en.wikipedia.org/).
sortByY xs = sortBy lowestY xs
             where lowestY a b = compare (y a, x a) (y b, x b)

pointAngle a b = (x b - x a) / (y b - y a)
 
pointOrdering a b = compare (pointAngle a b) 0.0
 
sortByAngle ps = bottomLeft : sortBy (compareAngles bottomLeft) (tail (sortedPs))
                where sortedPs = sortByY ps
                      bottomLeft = head (sortedPs)
  
compareAngles = comparing . pointAngle

grahamScan ps = scan (sortByAngle ps)
           where scan (a:v:b:xs) = if directionOf a v b == RIGHT
                                   then a : scan(b:xs)
                                   else a : scan(v:b:xs)
                 scan [a, b]      = [a, b]
                 scan _          = []

p0 = Point 2.1 2.0
p1 = Point 4.2 2.0
p2 = Point 0.5 2.5
p3 = Point 3.2 3.5
p4 = Point 1.2 4.0
p5 = Point 0.7 4.7
p6 = Point 1.0 1.0
p7 = Point 3.0 5.2
p8 = Point 4.0 4.0
p9 = Point 3.5 1.5
pA = Point 0.5 1.0
points = [p0,p1,p2,p3,p4,p5,p6,p7,p8,p9,pA]

-- Unused experimentations
distanceOf (ax, ay) (bx, by) = sqrt ( (bx - ax) ^ 2 + (by - ay) ^ 2 )

toDegree r = r * (180 / pi)

angleOf a v b = toDegree ( acos ( ( dav ^ 2 + dbv ^ 2 - dab ^ 2) / (2 * dav * dbv) ) )
        where dav = distanceOf a v
              dbv = distanceOf b v
              dab = distanceOf b a

