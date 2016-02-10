import Control.Applicative
import Control.Monad
import qualified Data.Set as Set
import Data.List (permutations, sortBy, sort, group)
import Debug.Trace
import System.Environment
import Data.Tree
import Data.Maybe

powerset :: [a] -> [[a]]
powerset = foldr (\x acc -> acc ++ (map ((:) x) acc)) [[]]

anagrams :: (String -> Bool) -> String -> [String]
anagrams p = removeDuplicates . filter p . permutations

-- Finds words in all possible subsets
anagramsS :: (String -> Bool) -> String -> [String]
anagramsS p x = 
    let xs = powerset x
        as = map (anagrams p) xs
    in removeDuplicates $ concat as

removeDuplicates = map head . group . sort

wordList :: IO [String]
wordList = lines
    <$> readFile "/usr/share/dict/words"

isWord :: IO (String -> Bool)
isWord = do
    w <- Set.fromList <$> wordList
    return $ flip Set.member $ w


main = do
    p  <- isWord
    xs <- getArgs
    xs `forM_` \x -> do
        putStr $ x ++ " -> "
        let as = anagramsS p x
        print $ sortBy (\a b -> compare (length a) (length b)) as

{-
main = do
    ws <- wordList
    let t = buildTree (map treeFromList ws)
    myutil t
-}
{-
mergeTree [] q = [q]
mergeTree (p@(Node a as):ps) q@(Node b bs) = if a == b
    then [Node a (foldl mergeTree bs as)]
    else p:(mergeTree ps q)
-}

-- merge :: Forest a -> Forest a -> Forest a
mergeForests [] q = q
mergeForests p [] = p
mergeForests (p@(Node a as):ps) (q@(Node b bs):qs) = if a == b
    then (Node a (mergeForests as bs)):(mergeForests ps qs)
    else q:(mergeForests (p:ps) qs)

mergeTrees p@(Node a as) q@(Node b bs) = if a == b
    then [Node a (mergeForests as bs)]
    else [p, q]

treeFromList = unfoldTree (\xs -> maybe (Nothing, []) (\x -> (Just x, [tail xs])) (listToMaybe xs))

buildTree :: Eq a => Forest a -> Forest a
buildTree = foldl (\acc x -> mergeForests acc [x]) []

foo = do
    t <- treeFromList "hej"
    (trace . show) t $ return "loool"

myutil :: Show a => Forest a -> IO ()
myutil = \x -> putStr $ drawForest $ (fmap (fmap show)) x

mtrees a b = myutil $ mergeTrees (treeFromList a) (treeFromList b)
