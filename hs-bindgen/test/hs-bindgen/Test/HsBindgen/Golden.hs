-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import Data.Text qualified as Text
import Test.Tasty

import Clang.Args
import Clang.Version
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.C.Predicate
  (DeclPredicate(..), HeaderPathPredicate(..), Predicate(..))
import HsBindgen.Config
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.C qualified as C
import Test.HsBindgen.Golden.Check.Exts qualified as Exts
import Test.HsBindgen.Golden.Check.FailingTrace qualified as FailingTrace
import Test.HsBindgen.Golden.Check.Hs qualified as Hs
import Test.HsBindgen.Golden.Check.PP qualified as PP
import Test.HsBindgen.Golden.Check.Rust qualified as Rust
import Test.HsBindgen.Golden.Check.TH qualified as TH

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources =
    testGroup "Test.HsBindgen.Golden" $
      map (testTreeFor testResources) testCases

testTreeFor :: IO TestResources -> TestCase -> TestTree
testTreeFor testResources test@TestCase{testHasOutput, testClangVersion}
  | Just versionPred <- testClangVersion
  , case clangVersion of
      ClangVersion version  -> not (versionPred version)
      ClangVersionUnknown _ -> True
  = testGroup (testName test) []

  | not testHasOutput
  = FailingTrace.check testResources test

  | otherwise
  = testGroup (testName test) [
        C.check           testResources test
      , Hs.check          testResources test
      , Exts.check        testResources test
      , TH.check          testResources test
      , PP.check          testResources test
      , BindingSpec.check testResources test
      , Rust.check        testResources test
      ]

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      --
      -- Standard test cases
      --

      defaultTest "adios"
    , defaultTest "anonymous"
    , defaultTest "bitfields"
    , defaultTest "bool"
    , defaultTest "distilled_lib_1"
    , defaultTest "enums"
    , defaultTest "enum_cpp_syntax"
    , defaultTest "fixedarray_arg"
    , defaultTest "fixedarray"
    , defaultTest "fixedwidth"
    , defaultTest "flam"
    , defaultTest "forward_declaration"
    , defaultTest "headers"
    , defaultTest "macro_functions"
    , defaultTest "macro_in_fundecl_vs_typedef"
    , defaultTest "macro_in_fundecl"
    , defaultTest "macro_typedef_scope"
    , defaultTest "macro_typedef_struct"
    , defaultTest "macro_types"
    , defaultTest "macros"
    , defaultTest "names"
    , defaultTest "nested_enums"
    , defaultTest "nested_types"
    , defaultTest "nested_unions"
    , defaultTest "opaque_declaration"
    , defaultTest "primitive_types"
    , defaultTest "recursive_struct"
    , defaultTest "redeclaration_identical"
    , defaultTest "simple_func"
    , defaultTest "simple_structs"
    , defaultTest "spec_examples"
    , defaultTest "struct_arg"
    , defaultTest "type_naturals"
    , defaultTest "typedef_vs_macro"
    , defaultTest "typedefs"
    , defaultTest "typenames"
    , defaultTest "unions"
    , defaultTest "uses_utf8"
    , defaultTest "vector"

      --
      -- Clang diagnostics
      --

    , testDiagnostic "attributes" $ \diag ->
        diagnosticCategoryText diag == "Nullability Issue"
    , testDiagnostic "unnamed-struct" $ \diag ->
        diagnosticCategoryText diag == "Semantic Issue"

      --
      -- Tests that require a trace predicate
      --

    , testTraceCustom "decls_in_signature" ["f3", "f4", "f5"] $ \case
        TraceFrontend (FrontendParse (ParseUnexpectedAnonInSignature info)) ->
          Just . Expected $ C.declId info
        TraceClang (ClangDiagnostic _diag) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , testTraceCustom "skip_over_long_double" ["fun1", "struct1"] $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType info UnsupportedLongDouble)) ->
          Just $ Expected $ C.declId info
        _otherwise ->
          Nothing
    , let declsWithMsgs :: [Labelled C.Name]
          declsWithMsgs = [
                Labelled "Renamed"  "struct1"
              , Labelled "Squashed" "struct1_t"
              , Labelled "Renamed"  "struct2"
              , Labelled "Squashed" "struct2_t"
              , Labelled "Renamed"  "struct3"
              , Labelled "Squashed" "struct3_t"
              , Labelled "Renamed"  "struct4"
              , Labelled "Squashed" "struct4_t"
              , Labelled "Renamed"  "struct6"
              , Labelled "Squashed" "struct8"
              , Labelled "Squashed" "struct9"
              , Labelled "Renamed"  "struct10"
              , Labelled "Squashed" "struct10_t"
              , Labelled "Renamed"  "struct11"
              , Labelled "Squashed" "struct11_t"
              , Labelled "Renamed"  "struct12"
              , Labelled "Squashed" "struct12_t"
              ]
      in testTraceCustom "typedef_analysis" declsWithMsgs $ \case
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsSquashed info)) ->
          Just $ Expected $ Labelled "Squashed" $ C.declIdName (C.declId info)
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsRenamedTagged info _to)) ->
          Just $ Expected $ Labelled "Renamed"  $ C.declIdName (C.declId info)
        _otherwise ->
          Nothing
    , testTraceSimple "varargs" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType _ UnsupportedVariadicFunction)) ->
          Just $ Expected ()
        _otherwise ->
          Nothing

      --
      -- Failing tests
      --

    , failingTestSimple "long_double" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType _ UnsupportedLongDouble)) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "implicit_fields_struct" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedImplicitFields {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "declaration_unselected_b" $ \case
        TraceFrontend (FrontendMangleNames (MangleNamesMissingDeclaration {})) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "fixedarray_res_a" $ \case
         TraceClang (ClangDiagnostic x) ->
           if "brackets are not allowed here" `Text.isInfixOf` diagnosticSpelling x
             then Just (Expected ())
             else Just Tolerated
         _otherwise ->
           Nothing
    , failingTestSimple "fixedarray_res_b" $ \case
        TraceClang (ClangDiagnostic x) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceClang _ ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "redeclaration_different" $ \case
        TraceFrontend (FrontendSort (SortErrorDeclIndex (Redeclaration {}))) ->
          Just (Expected ())
        TraceClang (ClangDiagnostic x) ->
          if "macro redefined" `Text.isInfixOf` diagnosticSpelling x
            then Just Tolerated
            else Nothing
        _otherwise ->
          Nothing
    , failingTestSimple "unsupported_builtin" $ \case
        TraceFrontend (FrontendParse (ParseUnsupportedType _info (UnsupportedBuiltin "__builtin_va_list"))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing

      --
      -- Miscellaneous other tests that require special treatment
      --

    , (defaultTest "bool_c23") {
          testClangVersion = Just (>= (15, 0, 0))
        }
    , (defaultTest "fun_attributes") {
          testClangVersion = Just (>= (15, 0, 0))
          -- TODO: <https://github.com/well-typed/hs-bindgen/issues/876>
          -- We are currently issueing a "non-extern non'static global" warning
          -- for @i@, which may not be correct @visibility@ is @hidden@.
        , testTracePredicate = customTracePredicate' ["my_printf", "i"] $ \case
             TraceFrontend (FrontendParse (ParseUnsupportedType info UnsupportedVariadicFunction)) ->
               Just $ Expected (C.declId info)
             TraceFrontend (FrontendParse (ParsePotentialDuplicateGlobal info)) ->
               Just $ Expected (C.declId info)
             _otherwise ->
               Nothing
        , testRustBindgen = RustBindgenFail
        }
    , (defaultTest "fun_attributes_conflict") {
          testTracePredicate = customTracePredicate [] $ \case
             TraceClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-ignored-attributes"}) ->
               Just Tolerated
             _otherwise ->
               Nothing
        }
    , let declsWithWarnings :: [C.PrelimDeclId]
          declsWithWarnings = [
                -- non-extern non-static globals
                "nesInteger"
              , "nesFloating"
              , "nesString1"
              , "nesString2"
              , "nesCharacter"
              , "nesParen"
              , "nesUnary"
              , "nesBinary"
              , "nesConditional"
              , "nesCast"
              , "nesCompound"
              , "nesInitList"
              , "nesBool"
              , "streamBinary"
              , "streamBinary_len"
              , "some_global_struct"
                -- Other warnings
              , "unusableAnon"
              ]
      in (defaultTest "globals") {
          -- Getting different output from (the same version of) rust-bindgen
          -- for this test on CI than locally. Unsure why, compiled against
          -- different llvm version? For now we just disable it.
          testRustBindgen    = RustBindgenIgnore
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendParse (ParsePotentialDuplicateGlobal info)) ->
              Just $ Expected (C.declId info)
            TraceFrontend (FrontendParse (ParseUnexpectedAnonInExtern info)) ->
              Just $ Expected (C.declId info)
            _otherwise ->
              Nothing
        }
    , (defaultTest "iterator") {
          testClangVersion = Just (>= (15, 0, 0))
        , testOnConfig     = \cfg -> cfg{
              configClangArgs = (configClangArgs cfg) {
                  clangEnableBlocks = True
                }
            }
        }
    , (defaultTest "macro_strings") {
          testRustBindgen = RustBindgenFail
        }
    , (defaultTest "named_vs_anon"){
          testClangVersion = Just (>= (19, 1, 0))
        }
    , (defaultTest "program_slicing_simple"){
          -- Check that program slicing generates bindings for uint32_t if we
          -- remove it from the standard external binding specification
          testOnConfig = \cfg -> cfg{
              configParsePredicate  = PTrue
            , configSelectPredicate = PIf (Left SelectFromMainHeaders)
            , configProgramSlicing  = EnableProgramSlicing
            }
        , testOnExtSpec = BindingSpec.deleteType C.QualName{
              qualNameName = "uint32_t"
            , qualNameKind = C.NameKindOrdinary
            }
        , testTracePredicate = customTracePredicate [
              "SelectedUInt32"
            ] $ \case
            TraceFrontend (FrontendSelect
                           (SelectSelected
                            (TransitiveDependencyOf
                             (C.NsPrelimDeclIdNamed nm _) _)))
              | nm == "uint32_t" -> Just $ Expected "SelectedUInt32"
            TraceFrontend (FrontendSelect (SelectSelected _)) -> Just Unexpected
            TraceFrontend (FrontendSelect (SelectExcluded _)) -> Just Tolerated
            _otherwise ->
              Nothing
        }
    , (defaultTest "program_slicing_selection"){
          testOnConfig = \cfg -> cfg{
              configParsePredicate  = PTrue
            , configSelectPredicate = POr
                (PIf . Right $ SelectByDeclName "FileOperationRecord")
                (PIf . Right $ SelectByDeclName "read_file_chunk")
            , configProgramSlicing  = EnableProgramSlicing
            }
        , testTracePredicate = customTracePredicate [
              "SelectedFileOpterationStatus"
            ] $ \case
            TraceFrontend (FrontendParse msg) -> case msg of
              -- TODO: Ideally, we do not see this warnings because they affect
              -- skipped declarations. See
              -- https://github.com/well-typed/hs-bindgen/issues/905.
              ParseUnsupportedType _ UnsupportedLongDouble       -> Just Tolerated
              ParseUnsupportedType _ UnsupportedVariadicFunction -> Just Tolerated
              ParseUnsupportedType _ (UnsupportedBuiltin _)      -> Just Tolerated
              ParseUnsupportedConst _                            -> Just Tolerated
              _other                                             -> Nothing
            TraceFrontend (FrontendSelect
                           (SelectSelected
                            (TransitiveDependencyOf
                             (C.NsPrelimDeclIdNamed nm _) _)))
              | nm == "FileOperationStatus" -> Just $ Expected "SelectedFileOpterationStatus"
            TraceFrontend (FrontendSelect (SelectSelected _)) -> Just Unexpected
            TraceFrontend (FrontendSelect (SelectExcluded _)) -> Just Tolerated
            _otherwise ->
              Nothing
          -- TODO: Also, we may want to specify an allow list; see
          -- https://github.com/well-typed/hs-bindgen/issues/907.
        , testRustBindgen = RustBindgenRun
        }
    , (defaultFailingTest "thread_local"){
          testClangVersion   = Just (>= (16, 0, 0))
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendParse (ParseUnsupportedTLS{})) ->
              Just $ Expected ()
            _otherwise ->
              Nothing
        }
    , (defaultTest "type_attributes") {
          testRustBindgen = RustBindgenFail
        }
    ]
