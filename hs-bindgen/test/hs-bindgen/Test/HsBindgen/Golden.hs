-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing)
import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.C qualified as C
import Test.HsBindgen.Golden.Check.Exts qualified as Exts
import Test.HsBindgen.Golden.Check.FailingTrace qualified as FailingTrace
import Test.HsBindgen.Golden.Check.Hs qualified as Hs
import Test.HsBindgen.Golden.Check.PP qualified as PP
import Test.HsBindgen.Golden.Check.Rust qualified as Rust
import Test.HsBindgen.Golden.Check.TH qualified as TH
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty

import Clang.Args
import Clang.Enum.Simple
import Clang.LowLevel.Core
import Clang.Version

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.AST.Internal qualified as C
import HsBindgen.Frontend.Naming qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.TraceMsg

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
  = withTestOutputDir $
    testGroup (testName test) [
        C.check           testResources test
      , Hs.check          testResources test
      , Exts.check        testResources test
      , TH.check          testResources test
      , PP.check          testResources test
      , BindingSpec.check testResources test
      , Rust.check        testResources test
      ]
  where
    withTestOutputDir k =
        withResource
          (createDirectoryIfMissing True (testOutputDir test))
          (\_ -> pure ())
          (\_ -> k)

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = manualTestCases ++ [
      --
      -- Standard test cases
      --

      defaultTest "declarations/declarations_required_for_scoping"
    , defaultTest "declarations/forward_declaration"
    , defaultTest "declarations/opaque_declaration"
    , defaultTest "declarations/redeclaration_identical"
    , defaultTest "documentation/data_kind_pragma"
    , defaultTest "edge-cases/adios"
    , defaultTest "edge-cases/distilled_lib_1"
    , defaultTest "edge-cases/flam"
    , (defaultTest "edge-cases/headers") {
          testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
              Just (Expected ())
            _otherwise -> Nothing
        }
    , defaultTest "edge-cases/names"
    , defaultTest "edge-cases/spec_examples"
    , defaultTest "edge-cases/uses_utf8"
    , defaultTest "functions/callbacks"
    , defaultTest "functions/circular_dependency_fun"
    , defaultTest "functions/simple_func"
    , defaultTest "macros/macro_functions"
    , defaultTest "macros/macros"
    , defaultTest "macros/macro_typedef_scope"
    , defaultTest "macros/macro_typedef_struct"
    , defaultTest "macros/macro_types"
    , defaultTest "types/anonymous"
    , defaultTest "types/bitfields"
    , defaultTest "types/bool"
    , defaultTest "types/circular_dependency_struct"
    , defaultTest "types/enum_cpp_syntax"
    , defaultTest "types/enums"
    , defaultTest "types/fixedwidth"
    , defaultTest "types/hsb_complex_test"
    , defaultTest "types/nested_enums"
    , defaultTest "types/nested_types"
    , defaultTest "types/nested_unions"
    , defaultTest "types/primitive_types"
    , defaultTest "types/recursive_struct"
    , defaultTest "types/simple_structs"
    , defaultTest "types/struct_arg"
    , defaultTest "types/typedef_vs_macro"
    , defaultTest "types/typenames"
    , defaultTest "types/type_qualifiers"
    , defaultTest "types/unions"
    , defaultTest "types/vector_test"

      --
      -- Clang diagnostics
      --

    , testDiagnostic "attributes/attributes" $ \diag ->
        diagnosticCategoryText diag == "Nullability Issue"
    , testTraceSimple "types/unnamed-struct" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic diag))
          | diagnosticCategoryText diag == "Semantic Issue" ->
            Just $ Expected ()
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise -> Nothing

      --
      -- Tests that require a trace predicate
      --

    , testTraceCustom "functions/decls_in_signature" ["f3", "f4", "f5"] $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _ ParseUnexpectedAnonInSignature)))) ->
          Just $ expectFromQualPrelimDeclId i
        TraceFrontend (FrontendClang (ClangDiagnostic _diag)) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , testTraceCustom "declarations/definitions" ["foo", "n"] $ \case
        TraceFrontend (FrontendSelect (SelectParseSuccess
          (AttachedParseMsg i _ _ (ParsePotentialDuplicateSymbol{})))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , let declsWithMsgs = [
              -- Duplicate symbols
              "bar1", "bar2", "bar3", "bar4"
            , "baz1", "baz2", "baz3"
            , "foo1", "foo2", "foo3"
            , "quux"
            , "wam"
            ]
      in testTraceCustom "macros/macro_in_fundecl" declsWithMsgs $ \case
        TraceFrontend (FrontendSelect (SelectParseSuccess
          (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , testTraceCustom "macros/macro_in_fundecl_vs_typedef" ["quux1", "quux2", "wam1", "wam2"] $ \case
        TraceFrontend (FrontendSelect (SelectParseSuccess
          (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , testTraceCustom "declarations/redeclaration" ["x", "n"] $ \case
        TraceFrontend (FrontendSelect (SelectParseSuccess
          (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
          Just $ expectFromQualPrelimDeclId i
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _
          (ParseUnknownStorageClass (unsafeFromSimpleEnum -> CX_SC_Static)))))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , let declsWithMsgs :: [C.QualPrelimDeclId]
          declsWithMsgs = [
                C.QualPrelimDeclIdNamed "stdin"  C.NameKindOrdinary
              , C.QualPrelimDeclIdNamed "stdout" C.NameKindOrdinary
              , C.QualPrelimDeclIdNamed "stderr" C.NameKindOrdinary
              ]
      in testTraceCustom "macros/macro_redefines_global" declsWithMsgs $ \case
        TraceFrontend (FrontendConstructTranslationUnit (ConstructTranslationUnitErrorDeclIndex (Redeclaration {redeclarationId = x}))) ->
          Just $ Expected x
        _otherwise ->
          Nothing
    , testTraceCustom "types/skip_over_long_double" ["fun1", "struct1"] $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _ (ParseUnsupportedType UnsupportedLongDouble))))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , testTraceCustom "declarations/tentative_definitions" ["i1", "i2", "i3", "i3"] $ \case
        TraceFrontend (FrontendSelect (SelectParseSuccess
          (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
          Just $ expectFromQualPrelimDeclId i
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{})))) ->
          Just $ expectFromQualPrelimDeclId i
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _
          (ParseUnknownStorageClass (unsafeFromSimpleEnum -> CX_SC_Static)))))) ->
          Just $ expectFromQualPrelimDeclId i
        TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
          Just Tolerated
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
      in testTraceCustom "types/typedef_analysis" declsWithMsgs $ \case
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsSquashed info)) ->
          Just $ Expected $ Labelled "Squashed" $ C.declIdName (C.declId info)
        TraceFrontend (FrontendHandleTypedefs (HandleTypedefsRenamedTagged info _to)) ->
          Just $ Expected $ Labelled "Renamed"  $ C.declIdName (C.declId info)
        _otherwise ->
          Nothing
    , testTraceCustom "types/typedefs" ["foo"] $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _ ParseFunctionOfTypeTypedef)))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , testTraceCustom "functions/varargs" ["f", "g"] $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg i _ _
          (ParseUnsupportedType UnsupportedVariadicFunction))))) ->
          Just $ expectFromQualPrelimDeclId i
        _otherwise ->
          Nothing
    , let declsWithWarnings = [
              -- *** Functions ***
              -- Problematic non-public visibility
              "f2" , "f3" , "f4"
            , "f12", "f13", "f14"

              -- Duplicate symbols
            , "f5" , "f6" , "f7" , "f8" , "f9"
            , "f15", "f16", "f17", "f18", "f19"

              -- *** Global variables ***
              -- Problematic non-public visibility
            , "i12", "i13", "i14"
              -- Duplicate symbols
            , "i0" , "i1" , "i2" , "i3" , "i4"
            , "i5" , "i6" , "i7" , "i8" , "i9"
            , "i15", "i16", "i17", "i18", "i19"
              -- Unsupported storage class
            , "i20", "i21", "i22", "i23", "i24"
            , "i25", "i26", "i27", "i28", "i29"
            ]
      in (defaultTest "attributes/visibility_attributes") {
          testOnFrontendConfig = \cfg -> cfg{
              frontendSelectPredicate =
                BAnd
                  (BIf (SelectHeader FromMainHeaders))
                  (BNot (BIf (SelectDecl DeclDeprecated)))
            }
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParseNonPublicVisibility))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseFailure
              (ParseFailure (AttachedParseMsg i _ _ (ParseUnknownStorageClass (unsafeFromSimpleEnum -> CX_SC_Static)))))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
              Just Tolerated
            _otherwise ->
              Nothing
        }

      --
      -- Failing tests
      --

    , failingTestSimple "types/failing/long_double" $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg _ _ _ (ParseUnsupportedType UnsupportedLongDouble))))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "types/failing/implicit_fields_struct" $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg _ _ _ ParseUnsupportedImplicitFields)))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestSimple "types/failing/implicit_fields_union" $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg _ _ _ ParseUnsupportedImplicitFields)))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing
    , failingTestCustom "declarations/failing/declaration_unselected_b" ["select" :: String] $ \case
        (TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable _ (_, UnavailableNotSelected) _))) ->
          Just $ Expected "select"
        _otherwise ->
          Nothing
    , failingTestSimple "declarations/failing/redeclaration_different" $ \case
        TraceFrontend (FrontendConstructTranslationUnit (ConstructTranslationUnitErrorDeclIndex (Redeclaration {}))) ->
          Just (Expected ())
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "macro redefined" `Text.isInfixOf` diagnosticSpelling x
            then Just Tolerated
            else Nothing
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestCustom "declarations/failing/tentative_definitions_linkage" [(), ()] $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "non-static declaration of" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "edge-cases/failing/unsupported_builtin" $ \case
        TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
          (AttachedParseMsg _ _ _
          (ParseUnsupportedType (UnsupportedBuiltin "__builtin_va_list")))))) ->
          Just $ Expected ()
        _otherwise ->
          Nothing

      -- Arrays
    , failingTestSimple "arrays/failing/array_res_1" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_2" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_3" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_4" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_5" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_6" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_7" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing
    , failingTestSimple "arrays/failing/array_res_8" $ \case
        TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
          if "function cannot return array type" `Text.isInfixOf` diagnosticSpelling x
            then Just (Expected ())
            else Nothing
        TraceFrontend (FrontendClang _) ->
          Just Tolerated
        TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
          Just Tolerated
        _otherwise ->
          Nothing

      --
      -- Miscellaneous other tests that require special treatment
      --

    , let declsWithWarnings = [
              -- Duplicate symbols
              "arr0"
            , "arr3"
            , "arr1"
            , "arr6"
              -- Unkown storage class: static non-const
            , "arr4"
            , "arr5"
            , "arr8"
            ]
      in (defaultTest "arrays/array") {
          testClangVersion = Just (>= (19, 0, 0))
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
               Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-extern-initializer"})) ->
               Just Tolerated
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-tentative-definition-array"})) ->
               Just Tolerated
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg i _ _
              (ParseUnknownStorageClass (unsafeFromSimpleEnum -> CX_SC_Static)))))) ->
               Just $ expectFromQualPrelimDeclId i
            _otherwise ->
               Nothing
        }
    , (defaultTest "program-analysis/delay_traces") {
          testOnFrontendConfig = \cfg -> cfg{
              frontendSelectPredicate =
                BOr
                  (BIf (SelectDecl (DeclNameMatches "_function")))
                  -- NOTE: Matching for name kind is not good practice, but we
                  -- want to check if nested, but skipped declarations are
                  -- correctly assigned name kinds.
                  (BIf (SelectDecl (DeclNameMatches "struct")))
            }
        , testTracePredicate = customTracePredicate' [
              "long_double_function"
            , "var_arg_function"
            , "long_double_s"
            , "nested_long_double_s"
            ] $ \case
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg i _ _ (ParseUnsupportedType UnsupportedLongDouble))))) ->
               Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg i _ _ (ParseUnsupportedType UnsupportedVariadicFunction))))) ->
               Just $ expectFromQualPrelimDeclId i
            _otherwise ->
               Nothing
        }
    , (defaultTest "edge-cases/asm") {
          testOnBootConfig = \cfg -> cfg {
              bootClangArgsConfig = (bootClangArgsConfig cfg) {
                  gnu = EnableGnu
                }
            }
        }
    , (defaultTest "binding-specs/binding_spec_simple"){
          testPrescriptiveBindingSpec = Just "examples/golden/binding-specs/binding_spec_simple.yaml"
        }
      -- Rust bindgen does not support non-float complex types.
    , (defaultTest "types/complex_non_float_test") {
          testRustBindgen = RustBindgenIgnore
      }
    , (defaultTest "documentation/doxygen_docs") {
          testClangVersion = Just (>= (19, 0, 0))
        }
    , (defaultTest "types/bool_c23") {
          testClangVersion = Just (>= (15, 0, 0))
        }
    , (defaultTest "functions/fun_attributes") {
          testClangVersion = Just (>= (15, 0, 0))
        , testTracePredicate = customTracePredicate' [
                "my_printf"
              , "i"
              , "f3"
              , "old_fn_deprecated"
              , "old_fn_unavailable"
              ] $ \case
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg i _ _ (ParseUnsupportedType UnsupportedVariadicFunction))))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParseNonPublicVisibility))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectDeprecated x)) ->
              Just $ expectFromDeclSelect x
            TraceFrontend (FrontendSelect (SelectParseNotAttempted (ParseNotAttempted
              (AttachedParseMsg n _ _ DeclarationUnavailable)))) ->
              Just $ expectFromQualPrelimDeclId n
            _otherwise ->
              Nothing
        , testRustBindgen = RustBindgenFail
        }
    , (defaultTest "functions/fun_attributes_conflict") {
          testTracePredicate = customTracePredicate [] $ \case
            TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-ignored-attributes"})) ->
              Just Tolerated
            _otherwise ->
              Nothing
        }
    , let declsWithWarnings :: [Text]
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
                -- Duplicate symbols
              , "classless"
              ]
      in (defaultTest "globals/globals") {
          -- Getting different output from (the same version of) rust-bindgen
          -- for this test on CI than locally. Unsure why, compiled against
          -- different llvm version? For now we just disable it.
          testRustBindgen    = RustBindgenIgnore
        , testTracePredicate = customTracePredicate' declsWithWarnings $ \case
            TraceFrontend (FrontendSelect (SelectParseSuccess
              (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
              Just $ expectFromQualPrelimDeclId i
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg i _ _ ParseUnexpectedAnonInExtern)))) ->
              Just $ expectFromQualPrelimDeclId i
            _otherwise ->
              Nothing
        }
    , (defaultTest "edge-cases/iterator") {
          testClangVersion     = Just (>= (15, 0, 0))
        , testOnBootConfig = \cfg -> cfg{
              bootClangArgsConfig = (bootClangArgsConfig cfg) {
                  enableBlocks = True
                }
            }
        }
    , (defaultTest "macros/macro_strings") {
          testRustBindgen = RustBindgenFail
        }
    , (defaultTest "types/named_vs_anon"){
          testClangVersion = Just (>= (19, 1, 0))
        }
    , (defaultTest "program-analysis/program_slicing_simple"){
          -- Check that program slicing generates bindings for uint32_t and
          -- uint64_t if we only provide external binding specifications for
          -- uint64_t.
          testOnFrontendConfig = \cfg -> cfg{
              frontendParsePredicate  = BTrue
            , frontendSelectPredicate = BIf (SelectHeader FromMainHeaders)
            , frontendProgramSlicing  = EnableProgramSlicing
            }
        , testStdlibSpec = BindingSpec.DisableStdlibBindingSpec
        , testExtBindingSpecs = [ "examples/golden/program-analysis/program_slicing_simple.yaml" ]
        , testTracePredicate = customTracePredicate [
              "selected foo"
            , "selected uint32_t"
            ] $ \case
            TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
              expectSelected decl.declInfo $ Set.singleton "foo"
            TraceFrontend (FrontendSelect (SelectStatusInfo (Selected TransitiveDependency) decl)) ->
              expectSelected decl.declInfo $ Set.singleton "uint32_t"
            _otherwise ->
              Nothing
        }
    , testTraceCustom "program-analysis/selection_fail" [
          "Fail"
        , "DependOnFailByValue"
        , "DependOnFailByReference"
        , "OkBefore", "OkAfter"
        ] $ \case
          TraceFrontend (FrontendSelect (SelectParseFailure _)) ->
            Just $ Expected "Fail"
          TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable _ (_, UnavailableParseFailed) decl)) ->
            Just $ expectFromDeclSelect decl
          TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
            Just $ expectFromDeclSelect decl
          _otherwise -> Nothing
    , (testVariant "program-analysis/selection_fail" "1.exclude_failed"){
          testOnFrontendConfig = \cfg -> cfg{
              frontendSelectPredicate = BAnd
                (       BIf $ SelectHeader   FromMainHeaders)
                (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
            }
        , testTracePredicate = customTracePredicate' [
              "DependOnFailByValue"
            , "DependOnFailByReference"
            , "OkBefore", "OkAfter"
            ] $ \case
              TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable _ (_, UnavailableNotSelected) decl)) ->
                Just $ expectFromDeclSelect decl
              TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
                Just $ expectFromDeclSelect decl
              _otherwise -> Nothing
        }
    , (testVariant "program-analysis/selection_fail" "2.program_slicing"){
          testOnFrontendConfig = \cfg -> cfg{
              frontendSelectPredicate = BAnd
                (       BIf $ SelectHeader   FromMainHeaders)
                (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
            , frontendProgramSlicing = EnableProgramSlicing
            }
        , testTracePredicate = customTracePredicate' [
              "DependOnFailByValue"
            , "DependOnFailByReference"
            , "OkBefore", "OkAfter"
            ] $ \case
              TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable _ (_, UnavailableParseFailed) decl)) ->
                Just $ expectFromDeclSelect decl
              TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
                Just $ expectFromDeclSelect decl
              _otherwise -> Nothing
        , testHasOutput = False
        }
    , (testVariant "program-analysis/selection_fail" "3.select_ok"){
          testOnFrontendConfig = \cfg -> cfg{
              frontendSelectPredicate = BAnd
                (       BIf $ SelectDecl $ DeclNameMatches "struct OkBefore")
                (BNot $ BIf $ SelectDecl $ DeclNameMatches "struct Fail")
            , frontendProgramSlicing = EnableProgramSlicing
            }
        , testTracePredicate = customTracePredicate' ["OkBefore"] $ \case
              TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
                Just $ expectFromDeclSelect decl
              _otherwise -> Nothing
        }
    , (defaultFailingTest "program-analysis/failing/selection_bad"){
          testTracePredicate = customTracePredicate ["size_t_select"] $ \case
            (TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable SelectionRoot (_, UnavailableNotSelected) _))) ->
              Just $ Expected "size_t_select"
            _other -> Nothing
        }
    , (defaultTest "program-analysis/selection_foo"){
          testTracePredicate = customTracePredicate' ["f"] $ \case
            (TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure m)))) ->
              Just $ expectFromQualPrelimDeclId m.declId
            _other -> Nothing
        }
    , (defaultTest "program-analysis/program_slicing_selection"){
          testOnFrontendConfig = \cfg -> cfg{
              frontendParsePredicate  = BTrue
            , frontendSelectPredicate = BOr
                (BIf . SelectDecl $ DeclNameMatches "FileOperationRecord")
                (BIf . SelectDecl $ DeclNameMatches "read_file_chunk")
            , frontendProgramSlicing  = EnableProgramSlicing
            }
        , testTracePredicate = customTracePredicate [
              "selected FileOperationRecord"
            , "selected FileOperationStatus"
            , "selected read_file_chunk"
              -- Macro redefines a global variable
            , "QualPrelimDeclIdNamed \"stdin\" NameKindOrdinary"
            , "QualPrelimDeclIdNamed \"stdout\" NameKindOrdinary"
            , "QualPrelimDeclIdNamed \"stderr\" NameKindOrdinary"
            ] $ \case
            TraceFrontend (FrontendSelect (SelectStatusInfo (Selected SelectionRoot) decl)) ->
              expectSelected decl.declInfo $ Set.fromList [
                  "FileOperationRecord"
                , "read_file_chunk"
                ]
            TraceFrontend (FrontendSelect (SelectStatusInfo (Selected TransitiveDependency) decl)) ->
              expectSelected decl.declInfo $ Set.singleton "FileOperationStatus"
            TraceFrontend (FrontendConstructTranslationUnit (ConstructTranslationUnitErrorDeclIndex (Redeclaration {redeclarationId = x}))) ->
              Just $ Expected (show x)
            _otherwise ->
              Nothing
          -- TODO: Also, we may want to specify an allow list; see
          -- https://github.com/well-typed/hs-bindgen/issues/907.
        , testRustBindgen = RustBindgenRun
        }
    , (defaultTest "program-analysis/reparse") {
          testClangVersion   = Just (>= (15, 0, 0)) -- parse 'bool'
        , testTracePredicate = customTracePredicate [] $ \case
            -- We don't care about the trace messages in this test
            _anything ->
              Just Tolerated
        }
    , (defaultTest "declarations/select_scoping") {
          testOnFrontendConfig = \cfg -> cfg {
            frontendParsePredicate = BIf (ParseHeader FromMainHeaders)
          , frontendSelectPredicate = BTrue
          }
        , testTracePredicate = customTracePredicate [
            "ParsedAndSelected2"
          , "ParsedAndSelected3"
          ] $ \case
            (TraceFrontend (FrontendSelect
              (TransitiveDependencyOfDeclarationUnavailable _
                (_, UnavailableNotSelected) _))) ->
                Just $ Expected "ParsedAndSelected2"
            (TraceFrontend (FrontendSelect
              (TransitiveDependencyOfDeclarationUnavailable _
                (_, UnavailableParseNotAttempted) _))) ->
                Just $ Expected "ParsedAndSelected3"
            _otherwise -> Nothing
        }
    , (defaultFailingTest "edge-cases/failing/thread_local"){
          testClangVersion   = Just (>= (16, 0, 0))
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
              (AttachedParseMsg _ _ _ ParseUnsupportedTLS)))) ->
              Just $ Expected ()
            _otherwise ->
              Nothing
        }
    , (defaultFailingTest "edge-cases/failing/select_no_match") {
          testOnFrontendConfig = \cfg -> cfg {
              frontendSelectPredicate = BIf (SelectDecl (DeclNameMatches "this_pattern_will_never_match"))
            }
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
              Just $ Expected ()
            _otherwise ->
              Nothing
        }
    , (defaultTest "attributes/type_attributes") {
          testRustBindgen = RustBindgenFail
        , testTracePredicate = singleTracePredicate $ \case
            TraceFrontend (FrontendSelect (SelectDeprecated _)) ->
              Just $ Expected ()
            _otherwise ->
              Nothing
        }
    ]
  where
    expectSelected ::
         C.DeclInfo Select
      -> Set C.Name
      -> Maybe (TraceExpectation String)
    expectSelected info expectedNames = case C.declIdName (C.declId info) of
      name
        | Set.member name expectedNames ->
            Just . Expected $ "selected " ++ Text.unpack (C.getName name)
        | otherwise -> Just Unexpected


-- | Test cases for header files used in the manual
manualTestCases :: [TestCase]
manualTestCases = [
      defaultTest "manual/arrays"
    , defaultTest "manual/function_pointers"
    , defaultTest "manual/zero_copy"
    ]

expectFromQualPrelimDeclId :: C.QualPrelimDeclId -> TraceExpectation Text
expectFromQualPrelimDeclId = Expected . \case
  C.QualPrelimDeclIdNamed   n _ -> C.getName n
  C.QualPrelimDeclIdAnon    n _ -> Text.pack $ show n
  C.QualPrelimDeclIdBuiltin n   -> C.getName n

expectFromDeclInfoSelect :: C.DeclInfo Select -> TraceExpectation Text
expectFromDeclInfoSelect = Expected . C.getName . C.declIdName . C.declId

expectFromDeclSelect :: C.Decl Select -> TraceExpectation Text
expectFromDeclSelect = expectFromDeclInfoSelect . C.declInfo
