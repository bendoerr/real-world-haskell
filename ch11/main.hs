import Test.QuickCheck
import Prettify2

main ::  IO ()
main = do quickCheck prop_empty_id
          quickCheck prop_empty_id
          quickCheck prop_char
          quickCheck prop_text
          quickCheck prop_line
          quickCheck prop_double
          quickCheck prop_hcat
          quickCheck prop_punctuate
