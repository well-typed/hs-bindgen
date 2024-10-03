module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import Test.QuickCheck ((===), counterexample)
import Text.EditDistance qualified as ED
import AnsiDiff qualified

main :: IO ()
main = defaultMain $ testGroup "ansi-diff"
    [ testProperty "metric-less-edit-distance" $ \xs ys ->
        let n = ED.levenshteinDistance ED.defaultEditCosts xs ys
            m = fst (AnsiDiff.metric xs ys)
        in counterexample (show (n, m)) $ m <= n

    , testProperty "metric-zero" $ \xs ys ->
        let n = ED.levenshteinDistance ED.defaultEditCosts xs ys
            m = fst (AnsiDiff.metric xs ys)
            f x = compare x 0
        in f n === f m
    ]
