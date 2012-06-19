mySum :: [Int] -> Int
mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

foldlSum xs = foldl sumIt 0 xs
    where sumIt acc x = acc + x

niceSum :: [Int] -> Int
niceSum xs = foldl (+) 0 xs
