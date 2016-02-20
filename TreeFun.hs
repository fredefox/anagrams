import TrieBuilder
import Data.Tree
import System.Environment
import Data.List
import Control.Monad

-- | Builds a suffix-tree from the words in the dictionary-file
getTrie :: IO (SuffixForest Char)
getTrie = fmap buildForest getWords

main :: IO ()
main = do
    t    <- getTrie
    args <- getArgs
    let res = map (anagrams t) args
    res `forM_` print

dictFile :: String
dictFile = "/usr/share/dict/words"
--dictFile = "100-words"

getWords :: IO [String]
getWords  = lines `fmap` readFile dictFile

anagramsT :: Eq a => SuffixTree a -> [a] -> [[a]]
-- The empty list has no anagrams
anagramsT _ [] = []
anagramsT (Node (a, b) _) [x] = [[x] | a == x && b]
-- If there are no valid words in our dictionary - then there are no anagrams!
-- anagrams [] _ = []
anagramsT t xs
    | r `elem` xs = map ((:) r) (anagramsF (subForest t) xs')
    | otherwise   = [] where
        r   = fst $ rootLabel t
        xs' = delete r xs

anagramsF :: Eq a => SuffixForest a -> [a] -> [[a]]
anagramsF f xs  = concatMap (`anagramsT` xs) f

anagrams :: Eq a => SuffixForest a -> [a] -> [[a]]
anagrams = anagramsF
