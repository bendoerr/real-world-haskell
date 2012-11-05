module Prettify (
        Doc (..),
        (<>),
        empty,
        char,
        double,
        line,
        fold,
        fsep,
        hcat,
        punctuate,
        text,
        compact,
        pretty
    ) where

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
         | Indent
          deriving (Show,Eq)

-- Concatinates Docs. Similar to ++ for strings.
(<>) :: Doc -> Doc -> Doc
Empty <> b = b
a <> Empty = a
a <> b     = a `Concat` b

empty :: Doc
empty = Empty

char :: Char -> Doc
char = Char

double :: Double -> Doc
double d = text (show d)

text :: String -> Doc
text "" = Empty
text s  = Text s

-- Creates a hard line break.
line :: Doc
line = Line

-- Concatinates a list of Docs.
hcat :: [Doc] -> Doc
hcat = fold (<>)

-- Helper. Fold helper for type safty.
fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- Concatinates a list of Docs.
fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

-- Helper. Adds a softline break if line gets too long.
softline :: Doc
softline = group line

-- Helper.
group :: Doc -> Doc
group x = flatten x `Union` x

-- Helper. Replaces Lines with spaces.
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

-- Puts a doc between every item in a list of Docs.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate _ [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds-- Helper. Encloses a doc between two characters.

compact :: Doc -> String
compact x = transform [x]
  where transform []   = ""
        transform (d:ds) = case d of
                           Empty        -> transform ds
                           Char c       -> c : transform ds
                           Text s       -> s ++ transform ds
                           Line         -> '\n' : transform ds
                           a `Concat` b -> transform (a:b:ds)
                           _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty w x = best 0 [x]
  where best c (d:ds) = case d of
                         Empty        -> best c ds
                         Char ch      -> ch : best (c + 1) ds
                         Text s       -> s ++ best (c + length s) ds
                         Line         -> '\n' : best 0 ds
                         a `Concat` b -> best c (a:b:ds)
                         a `Union` b  -> nicest c (best c (a:ds)) (best c (b:ds))
        best _ _      = ""
        nicest c a b | (w - least) `fits` a = a
                     | otherwise            = b
          where least = min w c

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

