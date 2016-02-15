module TrieBuilder (buildForest) where
import Data.Tree

buildForest :: Eq a => [[a]] -> Forest a
buildForest = foldl (flip addToForest) []

addToForest :: (Eq a) => [a] -> Forest a -> Forest a
addToForest []     ts     = ts
addToForest (x:xs) []     = [Node x $ addToForest xs []]
addToForest (x:xs) (t:ts)
    | x == rootLabel t  = t':ts
    | otherwise         = t : addToForest (x:xs) ts
    where
    t' = t { subForest = addToForest xs $ subForest t }
