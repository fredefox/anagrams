import Anagram
import System.Environment
import Control.Monad

main :: IO ()
main = do
    ana  <- fmap anagrams getWords
    args <- getArgs
    case args of
        [] -> forever $ interactive ana
        xs -> map ana xs `forM_` \as -> do
            as `forM_` (\a -> putStr (a ++ ", "))
            putStrLn ""

interactive :: (String -> [String]) -> IO ()
interactive ana = getLine >>= p . ana where
    p as = do
        as `forM_` \a -> putStr (a ++ ", ")
        putStrLn ""

dictFile :: String
dictFile = "/usr/share/dict/words"
--dictFile = "100-words"

getWords :: IO [String]
getWords  = lines `fmap` readFile dictFile
