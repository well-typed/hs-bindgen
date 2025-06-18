{-# LANGUAGE CPP #-}
module Main (main) where

import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.TreeDiff.Golden (ediffGolden1)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.Frontend (FrontendTrace (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (Redeclaration))
import HsBindgen.Frontend.Pass.MangleNames (MangleError (MissingDeclaration))
import HsBindgen.Frontend.Pass.Parse.IsPass (ParseTrace (..))
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException (..))
import HsBindgen.Frontend.Pass.Sort (SortError (..))
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

import Test.HsBindgen.C.Parser qualified
import Test.HsBindgen.Clang.Args qualified
import Test.HsBindgen.Util.Tracer qualified
import Test.Internal.Misc
import Test.Internal.Rust
import Test.Internal.TastyGolden (goldenTestSteps)
import Test.Internal.Tracer (withAnsiColor, withTracerTestCustom,
                             withWriterTracer)
import Test.Internal.TreeDiff.Orphans ()

#if __GLASGOW_HASKELL__ >=904
import Test.Internal.TH
#endif

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    packageRoot <- findPackageDirectory "hs-bindgen"
    defaultMain $ withRustBindgen $ \getRustBindgen ->
      withAnsiColor $ \getAnsiColor -> do
        tests packageRoot getAnsiColor getRustBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: FilePath -> IO AnsiColor -> IO FilePath -> TestTree
tests packageRoot getAnsiColor getRustBindgen =
  testGroup "test-internal" [
      Test.HsBindgen.C.Parser.tests getAnsiColor (argsWith [])
    , Test.HsBindgen.Clang.Args.tests getAnsiColor
    , Test.HsBindgen.Util.Tracer.tests
    , testGroup "examples/golden" $ map golden [
          "adios"
        , "anonymous"
        , "attributes"
        , "bitfields"
        , "bool"
        , "distilled_lib_1"
        , "enums"
        , "fixedarray"
        , "fixedwidth"
        , "flam"
        , "forward_declaration"
        , "headers"
        , "macro_functions"
        , "macro_in_fundecl_vs_typedef"
        , "macro_in_fundecl"
        , "macro_typedef_scope"
        , "macro_types"
        , "macros"
        , "manual_examples"
        , "names"
        , "nested_enums"
        , "nested_types"
        , "nested_unions"
        , "opaque_declaration"
        , "primitive_types"
        , "recursive_struct"
        , "redeclaration_identical"
        , "simple_func"
        , "simple_structs"
        , "struct_arg"
        , "type_naturals"
        , "typedef_vs_macro"
        , "typedefs"
        , "typenames"
        , "unions"
        , "unnamed-struct"
        , "uses_utf8"
        , "varargs"
        , "vector"
        ]
    -- @rs-bindgen@ panics on these
    , testGroup "examples/golden-norust" $ map goldenNoRust [
          "macro_strings"
        ]
    , testGroup "examples/failing" [
          expectTrace
            "long_double"
            "expected trace X"
            (\case
                TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedLongDouble)) -> True
                _otherwise -> False
            )
        , expectTrace
            "implicit_fields_struct"
            "expected trace UnsupportedImplicitFields"
            (\case
                TraceFrontend (FrontendParse (UnsupportedImplicitFields {})) -> True
                _otherwise -> False
            )
        , expectTrace
            "declaration_unselected_b"
            "expected trace MissingDeclaration"
            (\case
                TraceFrontend (FrontendNameMangler (MissingDeclaration {})) -> True
                _otherwise -> False
            )
        , expectTrace
            "redeclaration_different"
            "expected trace Redeclaration"
            (\case
                TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {}))) -> True
                _otherwise -> False
            )
        ]
    ]
  where
    argsWith :: [FilePath] -> ClangArgs
    argsWith includeDirs = getClangArgs packageRoot includeDirs

    golden :: TestName -> TestTree
    golden name =
      testGroup name $ goldenNoRust' name ++ [goldenRust getRustBindgen name]

    goldenNoRust :: TestName -> TestTree
    goldenNoRust name = testGroup name $ goldenNoRust' name

    goldenNoRust' :: TestName -> [TestTree]
    goldenNoRust' name = [
          goldenTreeDiff name
        , goldenHs name
        , goldenExtensions name
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh getAnsiColor packageRoot name
#endif
        , goldenPP name
        , goldenExtBindings name
        ]

    goldenTreeDiff :: TestName -> TestTree
    goldenTreeDiff name = do
      let target = "fixtures" </> (name ++ ".tree-diff.txt")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "treediff" target $ \report ->
        withOpts report $ \opts ->
          Pipeline.parseCHeaders opts [headerIncludePath]

    goldenHs :: TestName -> TestTree
    goldenHs name = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \report ->
        withOpts report $ \opts ->
          Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]

    goldenExtensions :: TestName -> TestTree
    goldenExtensions name = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \report ->
        withOpts report $ \opts -> do
          decls <-
            Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ unlines $ map show $ List.sort $ toList $
                Pipeline.genExtensions
              $ Pipeline.genSHsDecls decls

    goldenPP :: TestName -> TestTree
    goldenPP name = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \report ->
        withOpts report $ \opts -> do
          decls <- Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ Pipeline.preprocessPure ppOpts decls

    goldenExtBindings :: TestName -> TestTree
    goldenExtBindings name = do
      let target = "fixtures" </> (name ++ ".extbindings.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "extbindings" target $ \report ->
        withOpts report $ \opts -> do
          decls <-
            Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return . UTF8.toString . BindingSpec.encodeYaml $
            BindingSpec.genBindingSpec
              [headerIncludePath]
              (Hs.HsModuleName "Example")
              decls

    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    mkHeaderIncludePath :: String -> CHeaderIncludePath
    mkHeaderIncludePath = CHeaderQuoteIncludePath . (++ ".h")

    withOpts :: (String -> IO ()) -> (Opts -> IO a) -> IO a
    withOpts report action = withTracerTestCustom report getAnsiColor $
        \tracer' -> action $ (def :: Opts) {
            optsClangArgs = argsWith [ "examples/golden", "examples/golden-norust" ]
          , optsTracer = tracer'
          }

    ppOpts :: Pipeline.PPOpts
    ppOpts = def {
        Pipeline.ppOptsModule = HsModuleOpts { hsModuleOptsName = "Example" }
      }

    expectTrace :: TestName -> String -> (Trace -> Bool) -> TestTree
    expectTrace name msg predicate = testCase name $ do
      (_, traces) <- withWriterTracer $ \tracer -> do
             let headerIncludePath = mkHeaderIncludePath name
                 opts :: Opts
                 opts = def {
                     optsClangArgs = getClangArgs packageRoot [ "examples/failing" ]
                   , optsTracer = tracer
                   }
             Pipeline.translateCHeaders "failWithTraceTest" opts [headerIncludePath]
      assertBool msg $ any (predicate . tTrace) traces

