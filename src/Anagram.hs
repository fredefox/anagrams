module Anagram (anagrams) where
import TrieBuilder
import Data.Tree
import Data.List
import Data.Bool

anagramsT :: Eq a => SuffixTree a -> [a] -> [[a]]
-- The empty list has no anagrams
anagramsT _ [] = []
anagramsT (Node (a, b) _) [x] = [[x] | a == x && b]
anagramsT t xs
    -- The first line below includes partial sub-matches
    | r `elem` xs = bool id ([r]:) b
        $ map ((:) r) . anagramsF (subForest t) $ xs'
    | otherwise   = [] where
        r   = fst . rootLabel $ t
        b   = snd . rootLabel $ t
        xs' = delete r xs

anagramsF :: Eq a => SuffixForest a -> [a] -> [[a]]
anagramsF f xs  = concatMap (`anagramsT` xs) f

anagrams :: Eq a => [[a]] -> [a] -> [[a]]
anagrams = anagramsF . buildForest
