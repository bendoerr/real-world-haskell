-- Example of using Char byte strings
--
import qualified Data.ByteString.Lazy.Char8 as BS

closing = readPrice . (!!4) . BS.split ','

readPrice :: BS.ByteString -> Maybe Int
readPrice s =
    case BS.readInt s of
        Nothing              -> Nothing
        Just (dollars, rest) -> case BS.readInt (BS.tail rest) of
                                    Nothing         -> Nothing
                                    Just (cents, _) -> Just (dollars * 100 + cents)

highestClose = maximum . (Nothing:) . map closing . BS.lines

highestCloseFrom p = do
                     contents <- BS.readFile p
                     print (highestClose contents)

