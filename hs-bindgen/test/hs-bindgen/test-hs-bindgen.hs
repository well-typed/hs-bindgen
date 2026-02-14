module Main (main) where

import Test.Tasty

import Test.HsBindgen.Golden qualified as Golden
import Test.HsBindgen.Integration.ExitCode qualified as Integration.ExitCode
import Test.HsBindgen.Integration.OverwritePolicy qualified as Integration.OverwritePolicy
import Test.HsBindgen.Prop.Selection qualified as Prop.Selection
import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures qualified as THFixtures
import Test.HsBindgen.Unit.ClangArgs qualified as Unit.ClangArgs
import Test.HsBindgen.Unit.Tracer qualified as Unit.Tracer

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
          , Integration.OverwritePolicy.tests testResources
          ]
      , testGroup "property tests" [
            Prop.Selection.tests
          ]
      , testGroup "golden tests" [
            Golden.tests testResources
          ]
      , THFixtures.tests testResources
      ]
