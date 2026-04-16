module Test.CExpr.Parse (
    tests
  ) where

import Test.Tasty

import Test.CExpr.Parse.Golden    qualified as Golden
import Test.CExpr.Parse.Macro     qualified as Macro
import Test.CExpr.Parse.Type      qualified as Type

tests :: TestTree
tests = testGroup "parse" [
      Type.tests
    , Macro.tests
    , Golden.tests
    ]
