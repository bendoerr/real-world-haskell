-- file: ch04/SplitLines.hs
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = 
    let (pre, suf) = break isLineTerm cs
    in pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerm c = c == '\r' || c == '\n'
