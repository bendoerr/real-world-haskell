module BetterPredicate (
    -- * Types
      Predicate, InfoP
    -- * Main
    , betterFind
    -- * DSL
    , liftP, liftP2, constP, liftP', liftPath
    , pathP, sizeP
    , equalP, greaterP, lesserP,  andP, orP
    , (==?), (&&?), (>?), (<?)
) where

import Control.Exception (bracket, handle, IOException)
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.FilePath (takeExtension)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.Time (ClockTime(..))

import RecursiveContents (getRecursiveContents)

type Predicate = InfoP Bool

type InfoP a =  FilePath      -- ^ path to directory entry
             -> Permissions   -- ^ permissions
             -> Maybe Integer -- ^ file size (Nothing if not file)
             -> ClockTime     -- ^ last modified
             -> a

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
    where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe Integer)) $
    bracket (openFile path ReadMode) hClose $ \h -> do
        size <- hFileSize h
        return (Just size)

myTest path _ (Just size) _ =
    takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

equalP, (==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k
(==?) = equalP

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP, (>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)
(>?) = greaterP
(<?) = lesserP

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP, orP, (&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP = liftP2 (||)
(&&?) = andP

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP' q f k w x y z = f w x y z `q` constP k w x y z

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
