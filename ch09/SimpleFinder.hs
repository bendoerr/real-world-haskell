module SimpleFinder (
    -- * Main
     simpleFind
    -- * Examples
    , simpleFindCFiles
) where

import RecursiveContents (getRecursiveContents)
import System.FilePath (takeExtension)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
    names <- getRecursiveContents path
    return (filter p names)

simpleFindCFiles ::  FilePath -> IO [FilePath]
simpleFindCFiles = simpleFind ((== ".c") . takeExtension)
