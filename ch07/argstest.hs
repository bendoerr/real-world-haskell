-- Checking out System.Environment.getArgs
--
import System.Environment(getArgs,getProgName)

main = getProgName >>= putStrLn >> getArgs >>= (\args -> mapM_ putStrLn args)
