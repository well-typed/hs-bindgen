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
import Clang.Version
import HsBindgen.Backend.PP.Translation
import HsBindgen.BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.BindingSpec.Internal qualified as BindingSpec
import HsBindgen.C.Predicate (Predicate (..))
import HsBindgen.C.Reparse.Infra (ReparseError (..))
import HsBindgen.C.Tc.Macro (TcMacroError (TcErrors))
import HsBindgen.Clang (ClangMsg (..))
import HsBindgen.Config
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
import HsBindgen.Language.C.Name
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
       -- | Tests that require specific llvm version
    , testGroup "examples/golden/" $ map (uncurry golden) . mapMaybe checkVersion $ [
          ( "named_vs_anon"
          , (>= (19, 1, 0))
          , customTracePredicate [] $ \case
              TraceFrontend (FrontendHandleMacros _)
                -> Just Tolerated
              _otherwise
                -> Nothing
          )
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
            testConfig {
                 configPredicate      = SelectAll
               , configProgramSlicing = EnableProgramSlicing
               }
            -- Ensure that program slicing and external binding specification
            -- work well together. Remove `uint32_t` from the binding
            -- specifications, and select it using program slicing instead.
            (do
              extSpec <- getExtBindingSpec
              let uInt32T = QualName {
                      qualNameName = "uint32_t"
                    , qualNameKind = NameKindOrdinary
                    }
              pure $ Pipeline.BindingSpec {
                  bindingSpecUnresolved =
                      BindingSpec.BindingSpec
                    . Map.delete uInt32T
                    . BindingSpec.bindingSpecTypes
                    . Pipeline.bindingSpecUnresolved
                    $ extSpec
                , bindingSpecResolved =
                      BindingSpec.BindingSpec
                    . Map.delete uInt32T
                    . BindingSpec.bindingSpecTypes
                    . Pipeline.bindingSpecResolved
                    $ extSpec
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
    golden :: TestName -> TracePredicate TraceMsg -> TestTree
    golden name predicate = goldenWith name predicate testConfig getExtBindingSpec

    goldenWith ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenWith name predicate config getExtSpec =
      testGroup name $ goldenNoRust' name predicate config getExtSpec
                       ++ [goldenRust getRustBindgen name]

    goldenRustPanic :: TestName -> TracePredicate TraceMsg -> TestTree
    goldenRustPanic name predicate = testGroup name $
      rustExpectPanic getRustBindgen name :
      goldenNoRust' name predicate testConfig getExtBindingSpec

    goldenNoRust' ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> [TestTree]
    goldenNoRust' name predicate config getExtSpec = [
          goldenTreeDiff name predicate config getExtSpec
        , goldenHs name predicate config getExtSpec
        , goldenExtensions name predicate config getExtSpec
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name config (withBindgenResources predicate getExtSpec)
#endif
        , goldenPP name predicate config getExtSpec
        , goldenExtBindings name predicate config getExtSpec
        ]

    goldenTreeDiff ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenTreeDiff name predicate config getExtSpec = do
      let target = "fixtures" </> (name ++ ".tree-diff.txt")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "treediff" target $ \_ ->
        withBindgenResources predicate getExtSpec $ \tracer extSpec pSpec ->
          Pipeline.parseCHeaders tracer config extSpec pSpec [headerIncludePath]

    goldenHs ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenHs name predicate config getExtSpec = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \_ ->
        withBindgenResources predicate getExtSpec $ \tracer extSpec pSpec ->
          Pipeline.translateCHeaders
            "testmodule" tracer config extSpec pSpec [headerIncludePath]

    goldenExtensions ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenExtensions name predicate config getExtSpec = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \_ ->
        withBindgenResources predicate getExtSpec $ \tracer extSpec pSpec -> do
          decls <- Pipeline.translateCHeaders
            "testmodule" tracer config extSpec pSpec [headerIncludePath]
          return $ unlines $ map show $ List.sort $ toList $
                Pipeline.genExtensions
              $ Pipeline.genSHsDecls decls

    goldenPP ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenPP name predicate config getExtSpec = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \_ ->
        withBindgenResources predicate getExtSpec $ \tracer extSpec pSpec -> do
          decls <- Pipeline.translateCHeaders
            "testmodule" tracer config extSpec pSpec [headerIncludePath]
          return $ Pipeline.preprocessPure config decls

    goldenExtBindings ::
         TestName
      -> TracePredicate TraceMsg
      -> Config
      -> IO Pipeline.BindingSpec
      -> TestTree
    goldenExtBindings name predicate config getExtSpec = do
      let target = "fixtures" </> (name ++ ".bindingspec.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "bindingspec" target $ \_ ->
        withBindgenResources predicate getExtSpec $ \tracer extSpec pSpec -> do
          decls <-
            Pipeline.translateCHeaders
              "testmodule" tracer config extSpec pSpec [headerIncludePath]
          return . UTF8.toString . BindingSpec.encodeYaml $
            BindingSpec.genBindingSpec
              [headerIncludePath]
              (Hs.HsModuleName "Example")
              decls

    -- -<.> does weird stuff for filenames with multiple dots;
    -- I usually simply avoid using it.
    mkHeaderIncludePath :: String -> CHeaderIncludePath
    mkHeaderIncludePath = CHeaderQuoteIncludePath . (++ ".h")

    argsWith :: [FilePath] -> ClangArgs
    argsWith includeDirs = getClangArgs packageRoot includeDirs

    testConfig :: Config
    testConfig = def {
        configClangArgs = argsWith [
            "examples/golden"
          , "examples/golden-norust"
          ]
      , configHsModuleOpts = HsModuleOpts { hsModuleOptsName = "Example" }
      }

    failConfig :: Config
    failConfig = def {
        configClangArgs = argsWith [
            "examples/failing"
          ]
      }

    withBindgenResources
      :: TracePredicate TraceMsg
      -> IO Pipeline.BindingSpec
      -> (   Tracer IO TraceMsg
          -> ExternalBindingSpec
          -> PrescriptiveBindingSpec
          -> IO a)
      -> IO a
    withBindgenResources predicate getExtSpec action = do
      extSpec <- getExtSpec
      let pSpec = Pipeline.emptyBindingSpec
      withTracePredicate predicate $ \tracer -> action tracer extSpec pSpec

    expectTrace :: TestName -> TracePredicate TraceMsg -> TestTree
    expectTrace name predicate = testCase name $ do
      withBindgenResources predicate getExtBindingSpec $ \tracer extSpec pSpec -> do
        let headerIncludePath = mkHeaderIncludePath name
        void $ Pipeline.translateCHeaders
          "failWithTraceTest" tracer failConfig extSpec pSpec [headerIncludePath]

    checkVersion ::
         (TestName, (Int, Int, Int) -> Bool, TracePredicate TraceMsg)
      -> Maybe (TestName, TracePredicate TraceMsg)
    checkVersion (name, versionPred, tracePred) =
        case clangVersion of
          ClangVersion version | versionPred version -> Just (name, tracePred)
          _otherwise -> Nothing

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
