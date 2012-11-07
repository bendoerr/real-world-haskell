-- Exercise 1
-- Write a function that takes two arguments: a four-element tuple, and an
-- integer. With an integer argument of zero, it should return the leftmost
-- element of the tuple. With an argument of one, it should return the next
-- element. And so on. What restrictions do you have to put on the types of the
-- arguments in order to write a function that typechecks correctly?

-- Have to all have the same type to typecheck correctly.
exercise1 :: (a, a, a, a) -> Integer -> a
exercise1 (a, _, _, _) 0 = a
exercise1 (_, a, _, _) 1 = a
exercise1 (_, _, a, _) 2 = a
exercise1 (_, _, _, a) 3 = a
exercise1 _ _            = error "index out of bounds"


-- Exercise 2
-- Write a similar function that takes a six-tuple as its first argument.

-- Exercise 3
-- Try refactoring the two functions to share any common code you can identify. How much shared code are you able to you find?
exercise23 :: (a, a, a, a, a, a) -> Integer -> a
exercise23 (a0, a1, a2, a3, a4, a5) n
    | 0 <= n && n < 4 = exercise1 (a0, a1, a2, a3) n
    | n == 4    = a4
    | n == 5    = a5
    | otherwise = error "index out of bounds"
