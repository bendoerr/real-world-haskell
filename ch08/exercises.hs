-- Excercises for Chapter 8
--

import GlobRegex

-- Use ghci to explore what happens if you pass a malformed pattern, such as [,
-- to globToRegex. Write a small function that calls globToRegex, and pass it a
-- malformed pattern. What happens?

globToRegexFail = globToRegex "a["
-- An exception: unterminated character class happens.




-- While filesystems on Unix are usually case-sensitive (e.g., “G” vs. “g”) in
-- filenames, Windows filesystems are not. Add a parameter to the globToRegex
-- and matchesGlob functions that allows control over case sensitive matching

-- See GlobRegex.hs



-- Although we’ve gone to some lengths to write a portable namesMatching
-- function, the function uses our case sensitive globToRegex function. Find a
-- way to modify namesMatching to be case-sensitive on Unix, and case
-- insensitive on Windows, without modifying its type signature. (Hint:
-- consider reading the documentation for System.FilePath to look for a
-- variable that tells us whether we’re running on a Unix-like system or on
-- Windows.)

-- See Glob.hs (doMatch)




-- If you’re on a Unix-like system, look through the documentation for the
-- System.Posix.Files module, and see if you can find a replacement for the
-- doesNameExist function.

-- 
