-- Example of using a filter with interact
--
main = interact (unlines . filter (elem 'a') . lines)
