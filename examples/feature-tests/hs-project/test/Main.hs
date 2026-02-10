module Main (main) where

import Test.Tasty

import Test.Callbacks.Operators qualified
import Test.Callbacks.Stream qualified
import Test.Callbacks.View qualified

main :: IO ()
main = defaultMain $ testGroup "feature-tests" [
      Test.Callbacks.Operators.tests
    , Test.Callbacks.Stream.tests
    , Test.Callbacks.View.tests
    ]
