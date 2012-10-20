-- Find all files matching Glob.
-- Putting our IO and Regex skills to use.
--

-- |Clone of the glob function on UNIX type systems.
module Glob (
        namesMatching
    ) where

import System.Directory (
        doesDirectoryExist,
        doesFileExist,
        getCurrentDirectory,
        getDirectoryContents)
import System.FilePath (
        dropTrailingPathSeparator,
        splitFileName,
        (</>),
        isSearchPathSeparator)
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob, matchesGlobI)

-- |Finds directories and files that match the given glob.
namesMatching :: String -> IO [String]
namesMatching pattern
    | not (isPattern pattern) = do exists <- doesNameExist pattern
                                   return (if exists then [pattern] else [])
    | otherwise               = do case splitFileName pattern of
                                    ("",      baseName) -> do curDir <- getCurrentDirectory
                                                              listMatches curDir baseName
                                    (dirName, baseName) -> do dirs <- if isPattern dirName
                                                                      then namesMatching (dropTrailingPathSeparator dirName)
                                                                      else return [dirName]
                                                              let listDir = if isPattern baseName
                                                                            then listMatches
                                                                            else listPlain
                                                              pathNames <- forM dirs $ \dir -> do baseNames <- listDir dir baseName
                                                                                                  return (map (dir </>) baseNames)
                                                              return (concat pathNames)

-- |Helper
--   Does it look like a glob?
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

-- |Helper
--  Combines both doesFileExist and doesDirectoryExist.
doesNameExist :: FilePath -> IO Bool
doesNameExist name = do fileExists <- doesFileExist name
                        if fileExists
                        then return True
                        else doesDirectoryExist name

-- |Internal
--  Finds a list of all files matching the given glob pattern in a directory.
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pattern = do dirName' <- if null dirName
                                             then getCurrentDirectory
                                             else return dirName
                                 handle ((const (return [])) :: IOError -> IO [String]) $ do
                                    names <- getDirectoryContents dirName'
                                    let names' = if isHidden pattern
                                                 then filter isHidden names
                                                 else filter (not . isHidden) names
                                    return (filter (`doMatch` pattern) names')

-- |Helper
--  Starts with a dot it must be hidden.
isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- |Internal
--  Creates a singleton list or empty list depending on if the name exists.
listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do exists <- if null baseName
                                          then doesDirectoryExist dirName
                                          else doesNameExist (dirName </> baseName)
                                return (if exists then [baseName] else [])

-- |Internal
--  Uses case insensitive on *nix and case sensitive on Windows
doMatch :: FilePath -> String -> Bool
doMatch = if isNix
            then matchesGlobI
            else matchesGlob
    where isNix = isSearchPathSeparator ':'
