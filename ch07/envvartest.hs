-- Checking out environment variables
--
import System.Environment(getEnvironment, getEnv)

main = getEnvironment >>= mapM_ (\(key, val) -> putStrLn $ "Key: " ++ key ++ "\t\tValue: " ++ val)
