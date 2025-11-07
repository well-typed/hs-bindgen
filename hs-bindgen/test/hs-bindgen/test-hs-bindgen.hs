module Main (main) where

import Test.HsBindgen.Golden qualified as Golden
import Test.HsBindgen.Integration.ExitCode qualified as Integration.ExitCode
import Test.HsBindgen.Prop.Selection qualified as Prop.Selection
import Test.HsBindgen.Resources
import Test.HsBindgen.Unit.ClangArgs qualified as Unit.ClangArgs
import Test.HsBindgen.Unit.Tracer qualified as Unit.Tracer
import Test.Tasty

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
      , testGroup "integration tests" [
            Integration.ExitCode.tests testResources
          ]
      , testGroup "property tests" [
            Prop.Selection.tests
          ]
      , testGroup "golden tests" [
            Golden.tests testResources
          ]
      ]
