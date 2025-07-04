{-# LANGUAGE CPP #-}

module Main (main) where

import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.TreeDiff.Golden (ediffGolden1)
import System.FilePath ((</>))
import Test.Tasty (TestName, TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (testCase)

import Clang.Args
import Clang.HighLevel.Types (Diagnostic (diagnosticCategoryText, diagnosticSpelling))
import Clang.Paths
import HsBindgen.Backend.PP.Translation (HsModuleOpts (..))
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.C.Predicate (Predicate (..))
import HsBindgen.C.Reparse.Infra (ReparseError (..))
import HsBindgen.C.Tc.Macro (TcMacroError (TcErrors))
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Frontend (FrontendMsg (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (Redeclaration))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.HandleMacros (HandleMacrosMsg (..))
import HsBindgen.Frontend.Pass.MangleNames (MangleNamesMsg (..))
import HsBindgen.Frontend.Pass.Parse.IsPass (ParseMsg (..))
import HsBindgen.Frontend.Pass.Parse.Type.Monad (ParseTypeException (..))
import HsBindgen.Frontend.Pass.Slice (ProgramSlicing (..))
import HsBindgen.Frontend.Pass.Sort (SortMsg (..))
import HsBindgen.Imports
import HsBindgen.Language.C.Name (NameKind (..), QualName (..))
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Pipeline qualified as Pipeline
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Test.HsBindgen.C.Parser qualified
import Test.HsBindgen.C.Predicate qualified
import Test.HsBindgen.Clang.Args qualified
import Test.HsBindgen.Util.Tracer qualified
import Test.Internal.Misc
import Test.Internal.Rust
import Test.Internal.TastyGolden (goldenTestSteps)
import Test.Internal.Tracer
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
        initExtBindingSpec packageRoot $ \getExtBindingSpec ->
          tests packageRoot getExtBindingSpec getRustBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: FilePath -> IO Pipeline.BindingSpec -> IO FilePath -> TestTree
tests packageRoot getExtBindingSpec getRustBindgen =
  testGroup "test-internal" [
      Test.HsBindgen.C.Parser.tests (argsWith [])
    , Test.HsBindgen.C.Predicate.tests
    , Test.HsBindgen.Clang.Args.tests
    , Test.HsBindgen.Util.Tracer.tests
    , testGroup "examples/golden" $ map (uncurry golden) [
          ("adios"                       , defaultTracePredicate)
        , ("anonymous"                   , defaultTracePredicate)
        , ("attributes"                  ,
           singleTracePredicate $ \case
            TraceClang (ClangDiagnostic x)
              | diagnosticCategoryText x == "Nullability Issue"
              -> Just $ Expected ()
            _otherTrace
              -> Nothing
          )
        , ("bitfields"                   , defaultTracePredicate)
        , ("bool"                        , defaultTracePredicate)
        , ("decls_in_signature"          ,
            customTracePredicate ["'f1'", "'f2'", "'f3'", "'f4'", "'f5'"] $ \case
              TraceFrontend (FrontendParse (UnsupportedDeclsInSignature{unsupportedDeclsInSignatureFun}))
                -> Just . Expected . show . prettyForTrace $ C.declId unsupportedDeclsInSignatureFun
              TraceClang (ClangDiagnostic _diag)
                -> Just Tolerated
              _otherTrace
                -> Nothing
          )
        , ("distilled_lib_1"             ,
           customTracePredicate ["MacroErrorTc", "MacroErrorTc"] $ \case
            TraceFrontend (FrontendHandleMacros (MacroErrorTc (TcErrors _)))
              -> Just $ Expected "MacroErrorTc"
            _otherTrace
              -> Nothing
          )
        , ("enums"                       , defaultTracePredicate)
        , ("fixedarray"                  , defaultTracePredicate)
        , ("fixedwidth"                  , defaultTracePredicate)
        , ("flam"                        , defaultTracePredicate)
        , ("forward_declaration"         , defaultTracePredicate)
        , ("headers"                     , defaultTracePredicate)
        , ("macro_functions"             , defaultTracePredicate)
        , ("macro_in_fundecl_vs_typedef" , defaultTracePredicate)
        , ("macro_in_fundecl"            , defaultTracePredicate)
        , ("macro_typedef_scope"         , defaultTracePredicate)
        , ("macro_typedef_struct"        , defaultTracePredicate)
        , ("macro_types"                 , defaultTracePredicate)
        , ("macros"                      , defaultTracePredicate)
        , ("names"                       , defaultTracePredicate)
        , ("nested_enums"                , defaultTracePredicate)
        , ("nested_types"                , defaultTracePredicate)
        , ("nested_unions"               , defaultTracePredicate)
        , ("opaque_declaration"          , defaultTracePredicate)
        , ("primitive_types"             , defaultTracePredicate)
        , ("recursive_struct"            , defaultTracePredicate)
        , ("redeclaration_identical"     , defaultTracePredicate)
        , ("simple_func"                 , defaultTracePredicate)
        , ("simple_structs"              , defaultTracePredicate)
        , ("skip_over_long_double"       ,
           -- We expect a warning in two declarations
           customTracePredicate ["'fun1'", "'struct1'"] $ \case
            TraceFrontend (FrontendParse (UnsupportedType{
                  unsupportedTypeContext
                , unsupportedTypeException = UnsupportedLongDouble
                })) ->
              Just . Expected . show . prettyForTrace $
                C.declId unsupportedTypeContext
            _otherTrace ->
              Nothing
          )
        , ("spec_examples"               , defaultTracePredicate)
        , ("struct_arg"                  , defaultTracePredicate)
        , ("fixedarray_arg"              , defaultTracePredicate)
        , ("type_naturals"               , defaultTracePredicate)
        , ("typedef_vs_macro"            , defaultTracePredicate)
        , ("typedefs"                    , defaultTracePredicate)
        , ("typenames"                   , defaultTracePredicate)
        , ("unions"                      , defaultTracePredicate)
        , ("unnamed-struct"              ,
           singleTracePredicate $ \case
            TraceClang (ClangDiagnostic x) | diagnosticCategoryText x == "Semantic Issue"
              -> Just $ Expected ()
            _otherTrace
              -> Nothing
          )
        , ("uses_utf8"                   , defaultTracePredicate)
        , ("varargs"                     ,
           singleTracePredicate $ \case
            TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedVariadicFunction))
              -> Just $ Expected ()
            _otherTrace
              -> Nothing
          )
        , ("vector"                      , defaultTracePredicate)
        ]
    -- Tests that require special @hs-bindgen@ options.
    , testGroup "examples/golden/opts" [
          goldenWith "program_slicing"
            (singleTracePredicate $ \case
              (TraceFrontend (FrontendHandleMacros (MacroErrorReparse err)))
                | "Unexpected primitive type \"unsigned\"" `List.isInfixOf` (reparseError err)
                -> Just $ Expected ()
              _otherTrace
                -> Nothing
            )
            (\opts ->
               -- Ensure that program slicing and external binding specification
               -- work well together. Remove `uint32_t` from the binding
               -- specifications, and select it using program slicing instead.
               let uInt32T = QualName {
                       qualNameName = "uint32_t"
                     , qualNameKind = NameKindOrdinary
                     }
                   spec = Pipeline.BindingSpec {
                       bindingSpecUnresolved =
                           BindingSpec.BindingSpec
                         . Map.delete uInt32T
                         . BindingSpec.bindingSpecTypes
                         . Pipeline.bindingSpecUnresolved
                         $ Pipeline.optsExtBindingSpec opts
                     , bindingSpecResolved =
                           BindingSpec.BindingSpec
                         . Map.delete uInt32T
                         . BindingSpec.bindingSpecTypes
                         . Pipeline.bindingSpecResolved
                         $ Pipeline.optsExtBindingSpec opts
                     }
               in  opts {
                       Pipeline.optsPredicate      = SelectAll
                     , Pipeline.optsProgramSlicing = EnableProgramSlicing
                     , Pipeline.optsExtBindingSpec = spec
                     }
            )
       ]
    -- @rs-bindgen@ panics on these
    , testGroup "examples/golden-norust" $ map (uncurry goldenRustPanic) [
          ("macro_strings"   , defaultTracePredicate)
        , ("type_attributes" , defaultTracePredicate)
        ]
    , testGroup "examples/failing" [
          expectTrace
            "long_double"
            (singleTracePredicate $ \case
              TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedLongDouble))
                -> Just $ Expected ()
              _otherTrace
                -> Nothing
            )
        , expectTrace
            "implicit_fields_struct"
            (singleTracePredicate $ \case
              TraceFrontend (FrontendParse (UnsupportedImplicitFields {}))
                -> Just $ Expected ()
              _otherTrace
                -> Nothing
            )
        , expectTrace
            "declaration_unselected_b"
            (singleTracePredicate $ \case
              TraceFrontend (FrontendMangleNames (MissingDeclaration {}))
                -> Just $ Expected ()
              _otherTrace
                -> Nothing
            )
        , expectTrace
            "redeclaration_different"
            (singleTracePredicate $ \case
              TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {})))
                -> Just (Expected ())
              TraceClang (ClangDiagnostic x)
                | "macro redefined" `Text.isInfixOf` diagnosticSpelling x
                -> Just Tolerated
              _otherTrace
                -> Nothing
            )
        , expectTrace
            "fixedarray_res_a"
            (singleTracePredicate $ \case
              TraceClang (ClangDiagnostic x)
                | "brackets are not allowed here" `Text.isInfixOf` diagnosticSpelling x
                -> Just (Expected ())
              TraceClang _
                -> Just Tolerated
              _otherTrace
                -> Nothing
            )
        , expectTrace
            "fixedarray_res_b"
            (singleTracePredicate $ \case
              TraceClang (ClangDiagnostic x)
                | "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
                -> Just (Expected ())
              TraceClang _
                -> Just Tolerated
              _otherTrace
                -> Nothing
            )
        ]
    ]
  where
    argsWith :: [FilePath] -> ClangArgs
    argsWith includeDirs = getClangArgs packageRoot includeDirs

    golden :: TestName -> TracePredicate TraceMsg -> TestTree
    golden name predicate = goldenWith name predicate id

    goldenWith ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenWith name predicate changeOpts =
      testGroup name $ goldenNoRust' name predicate changeOpts
                       ++ [goldenRust getRustBindgen name]

    goldenRustPanic :: TestName -> TracePredicate TraceMsg -> TestTree
    goldenRustPanic name predicate = testGroup name $
      rustExpectPanic getRustBindgen name :
      goldenNoRust' name predicate id

    goldenNoRust' ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> [TestTree]
    goldenNoRust' name predicate changeOpts = [
          goldenTreeDiff name predicate changeOpts
        , goldenHs name predicate changeOpts
        , goldenExtensions name predicate changeOpts
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name (withOpts changeOpts predicate)
#endif
        , goldenPP name predicate changeOpts
        , goldenExtBindings name predicate changeOpts
        ]

    goldenTreeDiff ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenTreeDiff name predicate changeOpts = do
      let target = "fixtures" </> (name ++ ".tree-diff.txt")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "treediff" target $ \_ ->
        withOpts changeOpts predicate $ \opts ->
          Pipeline.parseCHeaders opts [headerIncludePath]

    goldenHs ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenHs name predicate changeOpts = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \_ ->
        withOpts changeOpts predicate $ \opts ->
          Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]

    goldenExtensions ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenExtensions name predicate changeOpts = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \_ ->
        withOpts changeOpts predicate $ \opts -> do
          decls <-
            Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ unlines $ map show $ List.sort $ toList $
                Pipeline.genExtensions
              $ Pipeline.genSHsDecls decls

    goldenPP ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenPP name predicate changeOpts = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \_ ->
        withOpts changeOpts predicate $ \opts -> do
          decls <- Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ Pipeline.preprocessPure ppOpts decls

    goldenExtBindings ::
         TestName
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> Pipeline.Opts)
      -> TestTree
    goldenExtBindings name predicate changeOpts = do
      let target = "fixtures" </> (name ++ ".bindingspec.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "bindingspec" target $ \_ ->
        withOpts changeOpts predicate $ \opts -> do
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

    withOpts ::
         (Pipeline.Opts -> Pipeline.Opts)
      -> TracePredicate TraceMsg
      -> (Pipeline.Opts -> IO a)
      -> IO a
    withOpts changeOpts predicate action = do
      extBindingSpec <- getExtBindingSpec
      withTracePredicate predicate $
        \tracer' -> action $ changeOpts $ (def :: Pipeline.Opts) {
            Pipeline.optsClangArgs      = argsWith [
                  "examples/golden"
                , "examples/golden-norust"
              ]
          , Pipeline.optsExtBindingSpec = extBindingSpec
          , Pipeline.optsTracer         = tracer'
          }

    ppOpts :: Pipeline.PPOpts
    ppOpts = def {
        Pipeline.ppOptsModule = HsModuleOpts { hsModuleOptsName = "Example" }
      }

    expectTrace :: TestName -> TracePredicate TraceMsg -> TestTree
    expectTrace name predicate = testCase name $ do
      withTracePredicate predicate $ \tracer -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts :: Pipeline.Opts
            opts = def {
                Pipeline.optsClangArgs =
                  getClangArgs packageRoot [ "examples/failing" ]
              , Pipeline.optsTracer = tracer
              }
        void $ Pipeline.translateCHeaders "failWithTraceTest" opts [headerIncludePath]

{-------------------------------------------------------------------------------
  Auxiliary functions
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
