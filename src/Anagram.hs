module Anagram (anagrams) where
import TrieBuilder
import Data.Tree
import Data.List

anagramsT :: Eq a => SuffixTree a -> [a] -> [[a]]
-- The empty list has no anagrams
anagramsT _ [] = []
anagramsT (Node (a, b) _) [x] = [[x] | a == x && b]
anagramsT t xs
    | r `elem` xs = map ((:) r) . anagramsF (subForest t) $ xs'
    | otherwise   = [] where
        r   = fst . rootLabel $ t
        xs' = delete r xs

anagramsF :: Eq a => SuffixForest a -> [a] -> [[a]]
anagramsF f xs  = concatMap (`anagramsT` xs) f

anagrams :: Eq a => [[a]] -> [a] -> [[a]]
anagrams = anagramsF . buildForest
