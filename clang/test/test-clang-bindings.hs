module Main (main) where

import Test.Tasty

import TestClangBindings.Traversal qualified as Traversal

main :: IO ()
main = defaultMain $ testGroup "test-clang-bindings" [
      Traversal.tests
    ]
