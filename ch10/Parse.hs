module Parse (
    -- Parsers
      identity, byte, char, peekByte, peekChar, while, whileWith, naturalNumber
    , skipSpaces, assert, bytes, rawPGM
    -- Parse
    , parse
) where

import qualified Data.ByteString.Lazy as BS

import Data.ByteString.Lazy (ByteString)
import Data.Char (chr, isDigit, isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

import PNM (Greymap (..))

-- |
data ParseState = ParseState { string :: ByteString -- ^
                             , offset :: Int64      -- ^
                             } deriving (Show)

-- | Hide the internal implementation details of our parsers.
newtype Parse a = Parse {
        -- | When we want to run the parsing function, we can just call the
        --   runParse accessors.
        runParse :: ParseState -> Either String (a, ParseState)
    }

instance Functor Parse where
    fmap f parser = parser ==> \r ->
                    identity (f r)

-- | Identity Parser that just returns whatever it is passed. Similar to the id
--   function.
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))

byte :: Parse Word8
byte = getState ==> \initState ->
        case BS.uncons (string initState) of
            Nothing -> bail "no more input"
            Just (b, r) ->
                putState newState ==> \_ ->
                    identity b
                where newState = initState { string = r, offset = newOffset }
                      newOffset = offset initState + 1

w2c :: Word8 -> Char
w2c = chr . fromIntegral

char :: Parse Char
char = w2c `fmap` byte

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . BS.uncons . string) `fmap` getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c `fmap` peekByte

while :: (Word8 -> Bool) -> Parse [Word8]
while p = (fmap p `fmap` peekByte) ==> \mp ->
          if mp == Just True
          then byte ==> \b ->
            (b:) `fmap` while p
          else identity []

whileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
whileWith f p = fmap f `fmap` while (p . f)

naturalNumber :: Parse Int
naturalNumber = whileWith w2c isDigit ==> \digits ->
                if null digits
                then bail "no more input"
                else let n = read digits
                     in if n < 0
                        then bail "integer overflow"
                        else identity n

skipSpaces :: Parse ()
skipSpaces = whileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True _ = identity ()
assert False err = bail err

bytes :: Int -> Parse ByteString
bytes n' = getState ==> \st ->
           let n = fromIntegral n'
               (h, t) = BS.splitAt n (string st)
               nst = st { offset = offset st + BS.length h, string = t }
           in putState nst
           ==>& assert (BS.length h == n) "end of input"
           ==>& identity h

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

-- | Move the offset.
-- modifyOffset :: ParseState -> Int64 -> ParseState
-- modifyOffset initState newOffset = initState { offset = newOffset }

bail :: String -> Parse a
bail err = Parse $ \s -> Left ("byte offset " ++ show (offset s) ++ ": " ++ err)

(==>) :: Parse a -> (a -> Parse b) -> Parse b
fp ==> sp = Parse chainedParser
    where chainedParser initState =
            case runParse fp initState of
                Left errMessage -> Left errMessage
                Right (fr, newState) -> runParse (sp fr) newState

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

-- |
parse :: Parse a -> ByteString -> Either String a
parse parser initState
    = case runParse parser (ParseState initState 0) of
        Left err          -> Left err
        Right (result, _) -> Right result

rawPGM :: Parse Greymap
rawPGM =    whileWith w2c (`notElem` " \r\n\t ") ==> \header -> skipSpaces
       ==>& assert (header == "P5") "invalid raw header"
       ==>& naturalNumber ==> \width -> skipSpaces
       ==>& naturalNumber ==> \height -> skipSpaces
       ==>& naturalNumber ==> \maxGrey -> byte
       ==>& bytes (width * height) ==> \bitmap ->
            identity (Greymap width height maxGrey bitmap)


