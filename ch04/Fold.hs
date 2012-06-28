foldl' _ zero []     = zero
foldl' f zero (x:xs) =
  let new = f zero x
  in  new `seq` foldl' f new xs
