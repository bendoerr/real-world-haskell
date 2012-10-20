-- File Name Matching and Regular Expressions in Haskell.
-- Translating a glob pattern into a regular expression.
--

-- |Match Glob patterns by convertng them to Regular Expressions.
module GlobRegex (
        globToRegex,
        globToRegexI,
        matchesGlob,
        matchesGlobI
    ) where

import Text.Regex.Posix ((=~))

-- |Convert a Glob Expression into a Regular Expression, anchor it to the
--  beginning and end of the line
globToRegex :: String -> String
globToRegex globex = '^' : globToRegex' globex ++ "$"

-- |Same as 'globToRegex' but case insensitive.
globToRegexI :: String -> String
globToRegexI globex = "(?i)^" ++ globToRegex' globex ++ "$"

-- |Checks if a filename matches a glob pattern by converting that glob pattern
--  to a regular expression and matching using that.
matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pattern = name =~ globToRegex pattern

-- |Same as 'matchesGlob' but case insensitive using 'globToRegexI'.
matchesGlobI :: FilePath -> String -> Bool
name `matchesGlobI` pattern = name =~ globToRegexI pattern

-- |Internal
--  Find glob specific characters, and convert them to regex specific
--  characters, escapes regex specific characters and verify that character
--  classes are properly terminated
globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) = ".*" ++ globToRegex' cs
globToRegex' ('?':cs) = '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "unterminated character class"
globToRegex' (c:cs) = escape c ++ globToRegex' cs

-- |Helper
--  Escape regex characters.
escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise           = [c]
    where regexChars = "\\+()^$.{}]"

-- |Helper
--  Verify character classes are terminated.
charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs)   = c : charClass cs
charClass []       = error "unterminated character class"
