data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False

stringEq :: [Char] -> [Char] -> Bool
stringEq []     []     = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _      _      = False
