module ControlledVisit (
      Info (..), getInfo
      , traverse
) where

import Control.Exception (handle, IOException)
import Control.Monad (liftM, forM)
import System.Directory (Permissions(..), getDirectoryContents, getModificationTime, getPermissions)
import System.FilePath ((</>))
import System.IO (IOMode(..), hFileSize, withFile)
import System.Time (ClockTime(..))

data Info = Info
          { infoPath :: FilePath
          , infoPerms :: Maybe Permissions
          , infoSize :: Maybe Integer
          , infoModTime :: Maybe ClockTime
          } deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
        perms <- maybeIO (getPermissions path)
        size <- maybeIO (withFile path ReadMode hFileSize)
        modified <- maybeIO (getModificationTime path)
        return (Info path perms size modified)
    where maybeIO :: IO a -> IO (Maybe a)
          maybeIO act = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe a)) (Just `liftM` act)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
    names <- getUsefulContents path 
    contents <- mapM getInfo (path : map (path </>) names)
    liftM concat $ forM (order contents) $ \info ->
        if isDirectory info && infoPath info /= path
            then traverse order (infoPath info)
            else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
