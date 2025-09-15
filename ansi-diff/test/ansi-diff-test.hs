module Main (main) where

import Test.QuickCheck (counterexample)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Text.EditDistance qualified as ED

import AnsiDiff ()

editDistanceNorm :: String -> String -> Int
editDistanceNorm xs ys = ED.levenshteinDistance ED.defaultEditCosts xs ys

-- | Lower bound for edit-distance:
--
-- consider two strings (lists) of separate lengths:
-- the length difference is the least amount of added (or removed) elements.
-- Only strings of equal length can be equal.
--
editDistanceNormLB :: [a] -> [a] -> Int
editDistanceNormLB xs ys = max n m - min n m
  where
    !n = length xs
    !m = length ys

main :: IO ()
main = defaultMain $ testGroup "ansi-diff"
    [ testProperty "edit-distance-lb" $ \xs ys ->
        let a = editDistanceNorm xs ys
            b = editDistanceNormLB xs ys
        in counterexample (show (a, b)) $ b <= a
    ]
