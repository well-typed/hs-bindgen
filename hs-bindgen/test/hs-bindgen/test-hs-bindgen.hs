module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup, withResource)

import Clang.Args
import HsBindgen.Pipeline qualified as Pipeline

import Test.Common.HsBindgen.TracePredicate
import Test.Common.Util.Cabal

import Test.HsBindgen.Orphans.TreeDiff ()
import Test.HsBindgen.Util.Clang
import Test.HsBindgen.Util.Rust

import Test.HsBindgen.Golden         qualified as Golden
import Test.HsBindgen.Prop.Selection qualified as Prop.Selection
import Test.HsBindgen.Unit.ClangArgs qualified as Unit.ClangArgs
import Test.HsBindgen.Unit.Tracer    qualified as Unit.Tracer

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $
      withRustBindgen                $ \getRustBindgen    ->
      initExtBindingSpec packageRoot $ \getExtBindingSpec ->
        tests packageRoot getExtBindingSpec getRustBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: FilePath -> IO Pipeline.BindingSpec -> IO FilePath -> TestTree
tests packageRoot getExtBindingSpec getRustBindgen =
    testGroup "test-hs-bindgen" [
        testGroup "unit tests" [
            Unit.Tracer.tests
          , Unit.ClangArgs.tests (argsWith [])
          ]
      , testGroup "property tests" [
            Prop.Selection.tests
          ]
      , testGroup "golden tests" [
            Golden.tests packageRoot getExtBindingSpec getRustBindgen
          ]
    ]
  where
    argsWith :: [FilePath] -> ClangArgs
    argsWith includeDirs = getClangArgs packageRoot includeDirs

{-------------------------------------------------------------------------------
  Test resources
-------------------------------------------------------------------------------}

initExtBindingSpec ::
     FilePath
  -> (IO Pipeline.BindingSpec -> TestTree)
  -> TestTree
initExtBindingSpec packageRoot =
    withResource getExtBindingSpec (const (return ()))
  where
    getExtBindingSpec :: IO Pipeline.BindingSpec
    getExtBindingSpec = withTracePredicate defaultTracePredicate $ \tracer ->
      let args = getClangArgs packageRoot []
      in  Pipeline.loadExtBindingSpecs
            tracer
            args
            Pipeline.UseStdlibBindingSpec
            []
