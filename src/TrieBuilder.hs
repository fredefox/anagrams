module TrieBuilder (buildForest, SuffixForest, SuffixTree) where

import Data.Tree

type SuffixForest a = Forest (a, Bool)
type SuffixTree   a = Tree   (a, Bool)

buildForest :: Eq a => [[a]] -> SuffixForest a
buildForest = foldl (flip addToForest) []

addToForest :: (Eq a) => [a] -> SuffixForest a -> SuffixForest a
addToForest []     ts     = ts
addToForest (x:xs) []     = [Node e $ addToForest xs []] where
    e = (x, null xs)
addToForest (x:xs) (t:ts)
    | x == (fst . rootLabel) t = t':ts
    | otherwise                = t : addToForest (x:xs) ts
    where
    t' = t
        { subForest = addToForest xs . subForest $ t
        , rootLabel = fmap (|| null xs) . rootLabel $ t
        }
