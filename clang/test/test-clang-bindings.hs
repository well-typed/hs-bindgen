module Main (main) where

import Test.Meta.IsConcrete qualified as IsConcrete
import Test.Tasty
import Test.Test.Exceptions qualified as Exceptions
import Test.Version qualified as Version

{-------------------------------------------------------------------------------
  Top-level
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "test-clang-bindings" [
      Version.tests
    , testGroup "Meta" [ -- Tests of the test infrastructure
          IsConcrete.tests
        ]
    , testGroup "Tests" [
          Exceptions.tests
        ]
    ]
