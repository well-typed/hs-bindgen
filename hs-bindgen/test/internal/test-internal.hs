{-# LANGUAGE CPP #-}

module Main (main) where

import Data.ByteString.UTF8 qualified as UTF8
import Data.List qualified as List
import Data.Text (isInfixOf)
import Data.TreeDiff.Golden (ediffGolden1)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup, withResource)
import Test.Tasty.HUnit (testCase)

import Clang.HighLevel.Types (Diagnostic (diagnosticCategoryText, diagnosticSpelling))
import Clang.Paths
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Gen qualified as BindingSpec
import HsBindgen.C.Tc.Macro (TcMacroError (TcErrors))
import HsBindgen.Frontend (FrontendTrace (..))
import HsBindgen.Frontend.Analysis.DeclIndex (DeclIndexError (Redeclaration))
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Pass.HandleMacros (MacroError (MacroErrorTc))
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
import Test.Internal.Tracer (TraceExpectation (Expected, Tolerated),
                             TracePredicate, customTracePredicate,
                             defaultTracePredicate, singleTracePredicate,
                             withTracePredicate)
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
        initExtBindings packageRoot $ \getExtBindings ->
          tests packageRoot getExtBindings getRustBindgen

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests ::
     FilePath
  -> IO ResolvedBindingSpec
  -> IO FilePath
  -> TestTree
tests packageRoot getExtBindings getRustBindgen =
  testGroup "test-internal" [
      Test.HsBindgen.C.Parser.tests (argsWith [])
    , Test.HsBindgen.Clang.Args.tests
    , Test.HsBindgen.Util.Tracer.tests
    , testGroup "examples/golden" $ map (uncurry golden) [
          ("adios"                       , defaultTracePredicate)
        , ("anonymous"                   , defaultTracePredicate)
        , ("attributes"                  ,
           singleTracePredicate "DiagnosticNullability" $ \case
            (TraceDiagnostic x) | diagnosticCategoryText x == "Nullability Issue"
              -> Just (Expected "DiagnosticNullability")
            _otherTrace -> Nothing
          )
        , ("bitfields"                   , defaultTracePredicate)
        , ("bool"                        , defaultTracePredicate)
        , ("distilled_lib_1"             ,
           customTracePredicate ["MacroErrorTc", "MacroErrorTc"] $ \case
            TraceFrontend (FrontendMacro (MacroErrorTc (TcErrors _)) )
              -> Just (Expected "MacroErrorTc")
            _otherTrace -> Nothing
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
        , ("manual_examples"             , defaultTracePredicate)
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
           customTracePredicate ["fun1", "struct1"] $ \case
            TraceFrontend (FrontendParse (UnsupportedType{
                  unsupportedTypeContext
                , unsupportedTypeException = UnsupportedLongDouble
                })) ->
              Just . Expected . prettyTrace $ C.declId unsupportedTypeContext
            _otherTrace ->
              Nothing
          )
        , ("struct_arg"                  , defaultTracePredicate)
        , ("type_naturals"               , defaultTracePredicate)
        , ("typedef_vs_macro"            , defaultTracePredicate)
        , ("typedefs"                    , defaultTracePredicate)
        , ("typenames"                   , defaultTracePredicate)
        , ("unions"                      , defaultTracePredicate)
        , ("unnamed-struct"              ,
           singleTracePredicate "DiagnosticSemanticIssue" $ \case
            (TraceDiagnostic x) | diagnosticCategoryText x == "Semantic Issue"
              -> Just (Expected "DiagnosticSemanticIssue")
            _otherTrace -> Nothing
          )
        , ("uses_utf8"                   , defaultTracePredicate)
        , ("varargs"                     ,
           singleTracePredicate "Variadic" $ \case
            TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedVariadicFunction))
              -> Just (Expected "Variadic")
            _otherTrace -> Nothing
          )
        , ("vector"                      , defaultTracePredicate)
        ]
    -- @rs-bindgen@ panics on these
    , testGroup "examples/golden-norust" $ map (uncurry goldenRustPanic) [
          ("macro_strings"   , defaultTracePredicate)
        , ("type_attributes" , defaultTracePredicate)
        ]
    , testGroup "examples/failing" [
          expectTrace
            "long_double"
            (singleTracePredicate "UnsupportedLongDouble" $ \case
              TraceFrontend (FrontendParse (UnsupportedType _ UnsupportedLongDouble))
                -> Just (Expected "UnsupportedLongDouble")
              _otherTrace -> Nothing
            )
        , expectTrace
            "implicit_fields_struct"
            (singleTracePredicate "UnsupportedImplicitFields" $ \case
              TraceFrontend (FrontendParse (UnsupportedImplicitFields {}))
                -> Just (Expected "UnsupportedImplicitFields")
              _otherTrace -> Nothing
            )
        , expectTrace
            "declaration_unselected_b"
            (singleTracePredicate "MissingDeclaration" $ \case
              TraceFrontend (FrontendNameMangler (MissingDeclaration {}))
                -> Just (Expected "MissingDeclaration")
              _otherTrace -> Nothing
            )
        , expectTrace
            "redeclaration_different"
            (singleTracePredicate "Redeclaration" $ \case
              TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {})))
                -> Just (Expected "Redeclaration")
              TraceDiagnostic x | "macro redefined" `isInfixOf` diagnosticSpelling x
                -> Just Tolerated
              _otherTrace -> Nothing
            )
        ]
    ]
  where
    argsWith :: [FilePath] -> ClangArgs
    argsWith includeDirs = getClangArgs packageRoot includeDirs

    golden :: TestName -> TracePredicate Trace -> TestTree
    golden name predicate =
      testGroup name $ goldenNoRust' name predicate ++ [goldenRust getRustBindgen name]

    goldenRustPanic :: TestName -> TracePredicate Trace -> TestTree
    goldenRustPanic name predicate = testGroup name $
      rustExpectPanic getRustBindgen name :
      goldenNoRust' name predicate

    goldenNoRust' :: TestName -> TracePredicate Trace -> [TestTree]
    goldenNoRust' name predicate = [
          goldenTreeDiff name predicate
        , goldenHs name predicate
        , goldenExtensions name predicate
-- Pretty-printing of TH differs between GHC versions; for example, @()@ becomes
-- @Unit@ in 9.8 <https://github.com/ghc-proposals/ghc-proposals/pull/475>.
-- We therefore test TH only with one specific GHC version.
#if __GLASGOW_HASKELL__ >=904
        , goldenTh packageRoot name predicate
#endif
        , goldenPP name predicate
        , goldenExtBindings name predicate
        ]

    goldenTreeDiff :: TestName -> TracePredicate Trace -> TestTree
    goldenTreeDiff name predicate = do
      let target = "fixtures" </> (name ++ ".tree-diff.txt")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "treediff" target $ \_ ->
        withOpts predicate $ \opts ->
          Pipeline.parseCHeaders opts [headerIncludePath]

    goldenHs :: TestName -> TracePredicate Trace -> TestTree
    goldenHs name predicate = do
      let target = "fixtures" </> (name ++ ".hs")
          headerIncludePath = mkHeaderIncludePath name
      ediffGolden1 goldenTestSteps "hs" target $ \_ ->
        withOpts predicate $ \opts ->
          Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]

    goldenExtensions :: TestName -> TracePredicate Trace -> TestTree
    goldenExtensions name predicate = do
      let target = "fixtures" </> (name ++ ".exts.txt")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "exts" target $ \_ ->
        withOpts predicate $ \opts -> do
          decls <-
            Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ unlines $ map show $ List.sort $ toList $
                Pipeline.genExtensions
              $ Pipeline.genSHsDecls decls

    goldenPP :: TestName -> TracePredicate Trace -> TestTree
    goldenPP name predicate = do
      let target = "fixtures" </> (name ++ ".pp.hs")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "pp" target $ \_ ->
        withOpts predicate $ \opts -> do
          decls <- Pipeline.translateCHeaders "testmodule" opts [headerIncludePath]
          return $ Pipeline.preprocessPure ppOpts decls

    goldenExtBindings :: TestName -> TracePredicate Trace -> TestTree
    goldenExtBindings name predicate = do
      let target = "fixtures" </> (name ++ ".extbindings.yaml")
          headerIncludePath = mkHeaderIncludePath name
      goldenVsStringDiff_ "extbindings" target $ \_ ->
        withOpts predicate $ \opts -> do
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

    withOpts :: TracePredicate Trace -> (Opts -> IO a) -> IO a
    withOpts predicate action = do
      extBindings <- getExtBindings
      withTracePredicate predicate $
        \tracer' -> action $ (def :: Opts) {
            optsClangArgs   = argsWith [ "examples/golden", "examples/golden-norust" ]
          , optsExtBindings = extBindings
          , optsTracer      = tracer'
          }

    ppOpts :: Pipeline.PPOpts
    ppOpts = def {
        Pipeline.ppOptsModule = HsModuleOpts { hsModuleOptsName = "Example" }
      }

    expectTrace :: TestName -> TracePredicate Trace -> TestTree
    expectTrace name predicate = testCase name $ do
      withTracePredicate predicate $ \tracer -> do
        let headerIncludePath = mkHeaderIncludePath name
            opts :: Opts
            opts = def {
                optsClangArgs = getClangArgs packageRoot [ "examples/failing" ]
              , optsTracer = tracer
              }
        void $ Pipeline.translateCHeaders "failWithTraceTest" opts [headerIncludePath]

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

initExtBindings ::
     FilePath
  -> (IO ResolvedBindingSpec -> TestTree)
  -> TestTree
initExtBindings packageRoot =
    withResource getExtBindings (const (return ()))
  where
    getExtBindings :: IO ResolvedBindingSpec
    getExtBindings = withTracePredicate defaultTracePredicate $ \tracer ->
      let args = getClangArgs packageRoot []
      in  snd <$> Pipeline.loadExtBindings tracer args True []
