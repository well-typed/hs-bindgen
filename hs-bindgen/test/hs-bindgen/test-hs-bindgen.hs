module Main (main) where

import Data.Sequence qualified as Seq
import Test.Tasty

import Test.HsBindgen.Clang.Macros.UniqueExpansion qualified as Clang.Macros.UniqueExpansion
import Test.HsBindgen.Frontend.LanguageC qualified as Frontend.LanguageC
import Test.HsBindgen.Frontend.Pass.PrepareReparse qualified as Frontend.Pass.PrepareReparse
import Test.HsBindgen.Golden qualified as Golden
import Test.HsBindgen.Integration.ExitCode qualified as Integration.ExitCode
import Test.HsBindgen.Integration.OverwritePolicy qualified as Integration.OverwritePolicy
import Test.HsBindgen.PPFixtures qualified as PPFixtures
import Test.HsBindgen.Prop.Selection qualified as Prop.Selection
import Test.HsBindgen.Resources
import Test.HsBindgen.THFixtures qualified as THFixtures
import Test.HsBindgen.Unit.ClangArgs qualified as Unit.ClangArgs
import Test.HsBindgen.Unit.Digraph qualified as Unit.Digraph
import Test.HsBindgen.Unit.Frontend qualified as Unit.Frontend
import Test.HsBindgen.Unit.Pretty qualified as Unit.Pretty
import Test.HsBindgen.Unit.Runtime qualified as Unit.Runtime
import Test.HsBindgen.Unit.Tracer qualified as Unit.Tracer

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $
    withTestResources $ \testResources ->
    testGroup "test-hs-bindgen" [
        Clang.Macros.UniqueExpansion.tests
      , Frontend.LanguageC.tests
      , Frontend.Pass.PrepareReparse.tests
      , testGroup "unit tests" [
            Unit.ClangArgs.tests testResources
          , Unit.Digraph.tests
          , Unit.Frontend.tests testResources
          , Unit.Tracer.tests
          , Unit.Pretty.tests
          , Unit.Runtime.tests
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
        -- Path from root to here, for @-p@ pattern filtering.
      , THFixtures.tests (Seq.fromList ["test-hs-bindgen"]) testResources
      , PPFixtures.tests (Seq.fromList ["test-hs-bindgen"]) testResources
      ]
