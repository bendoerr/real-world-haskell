-- Using a Lazy ByteString to determin if a file is ELF object
--
import qualified Data.ByteString.Lazy as BS

hasElfMagic :: BS.ByteString -> Bool
hasElfMagic c = BS.take 4 c == elfMagic
    where elfMagic = BS.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
                 content <- BS.readFile path
                 return (hasElfMagic content)


