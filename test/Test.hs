{-# language TemplateHaskell #-}

module Main where

import Anagram
import Test.QuickCheck
import Data.List
import Test.HUnit

{-| The found anagrams exists in the dictionary -}
prop_anagramsExist :: Eq a => [[a]] -> [a] -> Bool
prop_anagramsExist dict = all (`elem` dict) . anagrams dict

{-| An anagram of something contains the same elements in some order -}
prop_anagramInvariant :: Eq a => [[a]] -> [a] -> Bool
prop_anagramInvariant dict w = all (=~ w) . anagrams dict $ w where
    x =~ y = null $ x \\ y

{-| The order of the words in the dictionary is insignificant -}
test_orderDict :: Test
test_orderDict = TestCase
    $ assertEqual "\"a\" is an anagram over [\"ab\", \"a\"]"
    ["a"] $ anagrams ["ab", "a"] "a"

unitTests :: Test
unitTests = TestList
    [ test_orderDict
    ]

return []
runTests :: IO Bool
runTests = $quickCheckAll

-- Consider using Hspec [http://hspec.github.io/]
main :: IO ()
main = runTestTT unitTests >> runTests >> return ()
