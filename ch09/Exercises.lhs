> import Data.List (sortBy)
> import Data.Maybe (fromMaybe)
> import Data.Ord (comparing)
> import System.FilePath (takeExtension)
> import ControlledVisit

What should you pass to traverse to traverse a directory tree in reverse
alphabetic order?

> traverseReverseAlpha =  traverse (sortBy . flip $ comparing infoPath) "."


Using id as a control function, traverse id performs a preorder traversal of a
tree: it returns a parent directory before its children. Write a control
function that makes traverse perform a postorder traversal, in which it returns
children before their parent.

> traverseId = traverse id "."
> traverseReverse = traverse reverse "."


Take the predicates and combinators from the section called “Gluing predicates
together” and make them work with our new Info type.

> type InfoP a = Info -> a
>
> pathP :: InfoP FilePath
> pathP = infoPath
>
> sizeP :: InfoP Integer
> sizeP = fromMaybe (-1) . infoSize
>
> liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
> liftP op f k i = f i `op` k
>
> liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
> liftP2 op f g i = f i `op` g i
>
> constP :: a -> InfoP a
> constP k _ = k
>
> equalP, (==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
> equalP = liftP (==)
> (==?) = equalP
> infix 4 ==?
>
> greaterP, lesserP, (>?), (<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
> greaterP = liftP (>)
> (>?) = greaterP
> lesserP = liftP (<)
> (<?) = lesserP
> infix 4 >?
> infix 4 <?
>
> andP, orP, (&&?), (||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
> andP = liftP2 (&&)
> (&&?) = andP
> orP = liftP2 (||)
> (||?) = orP
> infixr 3 &&?
> infixr 3 ||?
>
> liftPath :: (FilePath -> a) -> InfoP a
> liftPath f = f . pathP
>
> theTest = liftPath takeExtension ==? ".hs" &&? sizeP >? 131072

Write a wrapper for traverse that lets you control traversal using one
predicate, and filter results using another.

> traverse' :: (Info -> Bool) -> ([Info] -> [Info]) -> FilePath -> IO [Info]
> traverse' f o = traverse $ o . filter f
