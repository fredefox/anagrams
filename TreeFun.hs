import TrieBuilder
import Data.Tree
import System.Environment
import Data.List

-- | Builds a suffix-tree from the words in the dictionary-file
getTrie :: IO (Forest Char)
getTrie = fmap buildForest getWords

main :: IO ()
main = getArgs >>= go where
    go :: [String] -> IO ()
    go [] = return ()
    go (x:_) = findAnagram x >>= print

findAnagram :: String -> IO [String]
findAnagram x = fmap (`anagrams` x) getTrie

dictFile :: String
dictFile = "/usr/share/dict/words"
--dictFile = "100-words"

getWords :: IO [String]
getWords  = lines `fmap` readFile dictFile

-- There is a bug in this implementation. It is due to the fact that cannot
-- determine if an arbitrary location in the tree marks the end of the word
-- by just seeing if there are no children from this point on. Minimal example:
-- if the dictionary is: ["a", "ab"], then clearly `anagrams t "a"` should
-- return `["a"]` but it wont.
anagramsT :: Eq a => Tree a -> [a] -> [[a]]
-- The empty list has no anagrams
anagramsT _ [] = []
anagramsT (Node a []) [x] = [[x] | a == x]
-- If there are no valid words in our dictionary - then there are no anagrams!
-- anagrams [] _ = []
anagramsT t xs
    | r `elem` xs = map ((:) r) (anagramsF (subForest t) xs')
    | otherwise   = [] where
        r   = rootLabel t
        xs' = delete r xs

anagramsF :: Eq a => Forest a -> [a] -> [[a]]
anagramsF f xs  = concatMap (`anagramsT` xs) f

anagrams :: Eq a => Forest a -> [a] -> [[a]]
anagrams = anagramsF
