module Main (main) where

import Test.Tasty

import Test.Meta.IsConcrete qualified as IsConcrete
import Test.Test.Exceptions qualified as Exceptions

main :: IO ()
main = defaultMain $ testGroup "test-clang-bindings" [
      testGroup "Meta" [ -- Tests of the test infrastructure
          IsConcrete.tests
        ]
    , testGroup "Tests" [
          Exceptions.tests
        ]
    ]
