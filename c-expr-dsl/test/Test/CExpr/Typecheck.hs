module Test.CExpr.Typecheck (
    tests
  ) where

import Test.Tasty

import Test.CExpr.Typecheck.Classify qualified as Classify

tests :: TestTree
tests = testGroup "typecheck" [
      Classify.tests
    ]
