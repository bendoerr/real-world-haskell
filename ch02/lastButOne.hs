lastButOne xs = if null xs
                then xs
                else drop (length xs - 2) (take (length xs -1) xs)
