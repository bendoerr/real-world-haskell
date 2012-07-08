module PrettifyStub where

import SimpleJSON

data Doc = ToBeDefined
           deriving (Show)

string :: String -> Doc
string s = undefined

text :: String -> Doc
text t = undefined

double :: Num -> Doc
double d = undefined


