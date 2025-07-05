module Main (main) where

import Test.Tasty

import Test.HsBindgen.Resources

import Test.HsBindgen.Golden         qualified as Golden
import Test.HsBindgen.Prop.Selection qualified as Prop.Selection
import Test.HsBindgen.Unit.ClangArgs qualified as Unit.ClangArgs
import Test.HsBindgen.Unit.Tracer    qualified as Unit.Tracer

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ withTestResources $ \testResources ->
    testGroup "test-hs-bindgen" [
        testGroup "unit tests" [
            Unit.Tracer.tests
          , Unit.ClangArgs.tests testResources
          ]
      , testGroup "property tests" [
            Prop.Selection.tests
          ]
      , testGroup "golden tests" [
            Golden.tests testResources
          ]
      ]
