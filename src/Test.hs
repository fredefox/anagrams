{-# language TemplateHaskell #-}

module Test where

import Anagram
import Test.QuickCheck
import Data.List

-- TODO: Should we arbitrarily constrain the test-cases to @String@s?

{-| The found anagrams exists in the dictionary -}
prop_anagramsExist :: [String] -> String -> Bool
prop_anagramsExist dict = all (`elem` dict) . anagrams dict

{-| An anagram of @s@ have contain the same elements in any order -}
prop_anagramInvariant :: [String] -> String -> Bool
prop_anagramInvariant dict w = all (=~ w) . anagrams dict $ w where
    x =~ y = null $ x \\ y

return []
runTests = $quickCheckAll
