module RecursiveContents (
    -- * Main
      getRecursiveContents
) where

import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad(forM)

{- | Recursivey list the contents of a directory and its subdirectories.
 -
 -   This is a bit of an impertive algorithm, the main loop checks to see if
 -   the current entry is a directory. If it is then we recurse otherwise just
 -   return the entry. -}
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", ".."]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path
            else return [path]
    return (concat paths)
