-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing)
import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.FailingTrace qualified as FailingTrace
import Test.HsBindgen.Golden.Check.PP qualified as PP
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
        TH.check          testResources test
      , PP.check          testResources test
      , BindingSpec.check testResources test
      ]
  where
    withTestOutputDir k =
        withResource
          (createDirectoryIfMissing True (testOutputDir test))
          (\_ -> pure ())
          (\_ -> k)

{-------------------------------------------------------------------------------
  Test case definitions
-------------------------------------------------------------------------------}

-- Arrays tests

test_arrays_array :: TestCase
test_arrays_array =
  let declsWithWarnings = [
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

-- Attributes tests

test_attributes_asm :: TestCase
test_attributes_asm = (defaultTest "attributes/asm") {
    testOnBootConfig = \cfg -> cfg {
        bootClangArgsConfig = (bootClangArgsConfig cfg) {
            gnu = EnableGnu
          }
      }
  }

test_attributes_type_attributes :: TestCase
test_attributes_type_attributes = (defaultTest "attributes/type_attributes") {
    testTracePredicate = singleTracePredicate $ \case
      TraceFrontend (FrontendSelect (SelectDeprecated _)) ->
        Just $ Expected ()
      _otherwise ->
        Nothing
  }

test_attributes_visibility_attributes :: TestCase
test_attributes_visibility_attributes =
  let declsWithWarnings = [
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

test_attributes_attributes :: TestCase
test_attributes_attributes =
  testDiagnostic "attributes/attributes" $ \diag ->
    diagnosticCategoryText diag == "Nullability Issue"

-- Binding specification tests

test_binding_specs_bs_ext_target_any :: TestCase
test_binding_specs_bs_ext_target_any =
  (defaultTest "binding-specs/bs_ext_target_any") {
      testExtBindingSpecs = [
          "examples/golden/binding-specs/bs_ext_target_any_e.yaml"
        ]
    }

test_binding_specs_bs_ext_target_mismatch :: TestCase
test_binding_specs_bs_ext_target_mismatch =
  (failingTestSimple "binding-specs/bs_ext_target_mismatch" aux) {
      testExtBindingSpecs = [
          "examples/golden/binding-specs/bs_ext_target_mismatch_e.yaml"
        ]
    }
  where
    aux = \case
      TraceBoot
        ( BootBindingSpec
            ( BindingSpecReadMsg
                (BindingSpec.BindingSpecReadIncompatibleTarget _)
            )
        ) -> Just $ Expected ()
      _otherwise -> Nothing

test_binding_specs_bs_pre_omit_type :: TestCase
test_binding_specs_bs_pre_omit_type =
  (defaultTest "binding-specs/bs_pre_omit_type") {
      testPrescriptiveBindingSpec =
        Just "examples/golden/binding-specs/bs_pre_omit_type_p.yaml"
    }

test_binding_specs_bs_pre_rename_type :: TestCase
test_binding_specs_bs_pre_rename_type =
  (defaultTest "binding-specs/bs_pre_rename_type") {
      testPrescriptiveBindingSpec =
        Just "examples/golden/binding-specs/bs_pre_rename_type_p.yaml"
    }

-- TODO target any with non-target-specific bindings is OK
-- test_binding_specs_bs_pre_target_any_ok :: TestCase

-- TODO target any with target-specific bindings traces error
-- test_binding_specs_bs_pre_target_any_bad :: TestCase

test_binding_specs_bs_pre_target_mismatch :: TestCase
test_binding_specs_bs_pre_target_mismatch =
  (failingTestSimple "binding-specs/bs_pre_target_mismatch" aux) {
      testPrescriptiveBindingSpec =
        Just "examples/golden/binding-specs/bs_pre_target_mismatch_p.yaml"
    }
  where
    aux = \case
      TraceBoot
        ( BootBindingSpec
            ( BindingSpecReadMsg
                (BindingSpec.BindingSpecReadIncompatibleTarget _)
            )
        ) -> Just $ Expected ()
      _otherwise -> Nothing

-- Declarations tests

test_declarations_declarations_required_for_scoping :: TestCase
test_declarations_declarations_required_for_scoping =
  defaultTest "declarations/declarations_required_for_scoping"

test_declarations_forward_declaration :: TestCase
test_declarations_forward_declaration =
  defaultTest "declarations/forward_declaration"

test_declarations_opaque_declaration :: TestCase
test_declarations_opaque_declaration =
  defaultTest "declarations/opaque_declaration"

test_declarations_redeclaration_identical :: TestCase
test_declarations_redeclaration_identical =
  defaultTest "declarations/redeclaration_identical"

test_documentation_data_kind_pragma :: TestCase
test_documentation_data_kind_pragma =
  defaultTest "documentation/data_kind_pragma"

test_edge_cases_adios :: TestCase
test_edge_cases_adios = defaultTest "edge-cases/adios"

test_edge_cases_distilled_lib_1 :: TestCase
test_edge_cases_distilled_lib_1 = defaultTest "edge-cases/distilled_lib_1"

test_edge_cases_flam :: TestCase
test_edge_cases_flam = defaultTest "edge-cases/flam"

test_edge_cases_headers :: TestCase
test_edge_cases_headers = (defaultTest "edge-cases/headers") {
    testTracePredicate = singleTracePredicate $ \case
      TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
        Just (Expected ())
      _otherwise -> Nothing
  }

test_edge_cases_names :: TestCase
test_edge_cases_names = defaultTest "edge-cases/names"

test_edge_cases_spec_examples :: TestCase
test_edge_cases_spec_examples = defaultTest "edge-cases/spec_examples"

test_edge_cases_uses_utf8 :: TestCase
test_edge_cases_uses_utf8 = defaultTest "edge-cases/uses_utf8"

test_functions_callbacks :: TestCase
test_functions_callbacks = defaultTest "functions/callbacks"

test_functions_circular_dependency_fun :: TestCase
test_functions_circular_dependency_fun =
  defaultTest "functions/circular_dependency_fun"

test_functions_simple_func :: TestCase
test_functions_simple_func = defaultTest "functions/simple_func"

test_macros_macro_functions :: TestCase
test_macros_macro_functions = defaultTest "macros/macro_functions"

test_macros_macros :: TestCase
test_macros_macros = defaultTest "macros/macros"

test_macros_macro_typedef_scope :: TestCase
test_macros_macro_typedef_scope = defaultTest "macros/macro_typedef_scope"

test_macros_macro_typedef_struct :: TestCase
test_macros_macro_typedef_struct = defaultTest "macros/macro_typedef_struct"

test_macros_macro_types :: TestCase
test_macros_macro_types = defaultTest "macros/macro_types"

test_types_structs_anonymous :: TestCase
test_types_structs_anonymous = defaultTest "types/structs/anonymous"

test_types_structs_bitfields :: TestCase
test_types_structs_bitfields = defaultTest "types/structs/bitfields"

test_types_primitives_bool :: TestCase
test_types_primitives_bool = defaultTest "types/primitives/bool"

test_types_structs_circular_dependency_struct :: TestCase
test_types_structs_circular_dependency_struct =
  defaultTest "types/structs/circular_dependency_struct"

test_types_enums_enum_cpp_syntax :: TestCase
test_types_enums_enum_cpp_syntax =
  defaultTest "types/enums/enum_cpp_syntax"

test_types_enums_enums :: TestCase
test_types_enums_enums = defaultTest "types/enums/enums"

test_types_primitives_fixedwidth :: TestCase
test_types_primitives_fixedwidth = defaultTest "types/primitives/fixedwidth"

test_types_complex_hsb_complex_test :: TestCase
test_types_complex_hsb_complex_test =
  defaultTest "types/complex/hsb_complex_test"

test_types_enums_nested_enums :: TestCase
test_types_enums_nested_enums = defaultTest "types/enums/nested_enums"

test_types_nested_nested_types :: TestCase
test_types_nested_nested_types = defaultTest "types/nested/nested_types"

test_types_unions_nested_unions :: TestCase
test_types_unions_nested_unions = defaultTest "types/unions/nested_unions"

test_types_primitives_primitive_types :: TestCase
test_types_primitives_primitive_types =
  defaultTest "types/primitives/primitive_types"

test_types_structs_recursive_struct :: TestCase
test_types_structs_recursive_struct =
  defaultTest "types/structs/recursive_struct"

test_types_structs_simple_structs :: TestCase
test_types_structs_simple_structs = defaultTest "types/structs/simple_structs"

test_types_structs_struct_arg :: TestCase
test_types_structs_struct_arg = defaultTest "types/structs/struct_arg"

test_types_typedefs_typedef_vs_macro :: TestCase
test_types_typedefs_typedef_vs_macro =
  defaultTest "types/typedefs/typedef_vs_macro"

test_types_typedefs_typenames :: TestCase
test_types_typedefs_typenames = defaultTest "types/typedefs/typenames"

test_types_qualifiers_type_qualifiers :: TestCase
test_types_qualifiers_type_qualifiers =
  defaultTest "types/qualifiers/type_qualifiers"

test_types_unions_unions :: TestCase
test_types_unions_unions = defaultTest "types/unions/unions"

test_types_complex_vector_test :: TestCase
test_types_complex_vector_test = defaultTest "types/complex/vector_test"

test_types_structs_unnamed_struct :: TestCase
test_types_structs_unnamed_struct =
  testTraceSimple "types/structs/unnamed-struct" $ \case
    TraceFrontend (FrontendClang (ClangDiagnostic diag))
      | diagnosticCategoryText diag == "Semantic Issue" ->
        Just $ Expected ()
    TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
      Just Tolerated
    _otherwise -> Nothing

test_functions_decls_in_signature :: TestCase
test_functions_decls_in_signature =
  testTraceCustom "functions/decls_in_signature" ["f3", "f4", "f5"] $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg i _ _ ParseUnexpectedAnonInSignature)))) ->
      Just $ expectFromQualPrelimDeclId i
    TraceFrontend (FrontendClang (ClangDiagnostic _diag)) ->
      Just Tolerated
    _otherwise ->
      Nothing

test_declarations_definitions :: TestCase
test_declarations_definitions =
  testTraceCustom "declarations/definitions" ["foo", "n"] $ \case
    TraceFrontend (FrontendSelect (SelectParseSuccess
      (AttachedParseMsg i _ _ (ParsePotentialDuplicateSymbol{})))) ->
      Just $ expectFromQualPrelimDeclId i
    _otherwise ->
      Nothing

test_macros_macro_in_fundecl :: TestCase
test_macros_macro_in_fundecl =
  let declsWithMsgs = [
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

test_macros_macro_in_fundecl_vs_typedef :: TestCase
test_macros_macro_in_fundecl_vs_typedef =
  testTraceCustom "macros/macro_in_fundecl_vs_typedef" ["quux1", "quux2", "wam1", "wam2"] $ \case
    TraceFrontend (FrontendSelect (SelectParseSuccess
      (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
      Just $ expectFromQualPrelimDeclId i
    _otherwise ->
      Nothing

test_declarations_redeclaration :: TestCase
test_declarations_redeclaration = testTraceCustom "declarations/redeclaration" ["x", "n"] $ \case
  TraceFrontend (FrontendSelect (SelectParseSuccess
    (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
    Just $ expectFromQualPrelimDeclId i
  TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
    (AttachedParseMsg i _ _
    (ParseUnknownStorageClass (unsafeFromSimpleEnum -> CX_SC_Static)))))) ->
    Just $ expectFromQualPrelimDeclId i
  _otherwise ->
    Nothing

test_macros_macro_redefines_global :: TestCase
test_macros_macro_redefines_global =
  let declsWithMsgs :: [C.QualPrelimDeclId]
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

test_types_special_parse_failure_long_double :: TestCase
test_types_special_parse_failure_long_double =
  testTraceCustom "types/special/parse_failure_long_double" ["fun1", "struct1"] $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg i _ _ (ParseUnsupportedType UnsupportedLongDouble))))) ->
      Just $ expectFromQualPrelimDeclId i
    _otherwise ->
      Nothing

test_declarations_tentative_definitions :: TestCase
test_declarations_tentative_definitions =
  testTraceCustom "declarations/tentative_definitions" ["i1", "i2", "i3", "i3"] $ \case
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

test_types_typedefs_typedef_analysis :: TestCase
test_types_typedefs_typedef_analysis =
  let declsWithMsgs :: [Labelled C.Name]
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
  in testTraceCustom "types/typedefs/typedef_analysis" declsWithMsgs $ \case
    TraceFrontend (FrontendHandleTypedefs (HandleTypedefsSquashed info)) ->
      Just $ Expected $ Labelled "Squashed" $ C.declIdName (C.declId info)
    TraceFrontend (FrontendHandleTypedefs (HandleTypedefsRenamedTagged info _to)) ->
      Just $ Expected $ Labelled "Renamed"  $ C.declIdName (C.declId info)
    _otherwise ->
      Nothing

test_types_typedefs_typedefs :: TestCase
test_types_typedefs_typedefs = testTraceCustom "types/typedefs/typedefs" ["foo"] $ \case
  TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
    (AttachedParseMsg i _ _ ParseFunctionOfTypeTypedef)))) ->
    Just $ expectFromQualPrelimDeclId i
  _otherwise ->
    Nothing

test_functions_varargs :: TestCase
test_functions_varargs = testTraceCustom "functions/varargs" ["f", "g"] $ \case
  TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
    (AttachedParseMsg i _ _
    (ParseUnsupportedType UnsupportedVariadicFunction))))) ->
    Just $ expectFromQualPrelimDeclId i
  _otherwise ->
    Nothing

test_types_failing_long_double :: TestCase
test_types_failing_long_double =
  failingTestSimple "types/failing/long_double" $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg _ _ _ (ParseUnsupportedType UnsupportedLongDouble))))) ->
      Just $ Expected ()
    _otherwise ->
      Nothing

test_types_failing_implicit_fields_struct :: TestCase
test_types_failing_implicit_fields_struct =
  failingTestSimple "types/failing/implicit_fields_struct" $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg _ _ _ ParseUnsupportedImplicitFields)))) ->
      Just $ Expected ()
    _otherwise ->
      Nothing

test_types_failing_implicit_fields_union :: TestCase
test_types_failing_implicit_fields_union =
  failingTestSimple "types/failing/implicit_fields_union" $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg _ _ _ ParseUnsupportedImplicitFields)))) ->
      Just $ Expected ()
    _otherwise ->
      Nothing

test_declarations_failing_declaration_unselected_b :: TestCase
test_declarations_failing_declaration_unselected_b =
  failingTestCustom "declarations/failing/declaration_unselected_b" ["select" :: String] $ \case
    (TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable _ (_, UnavailableNotSelected) _))) ->
      Just $ Expected "select"
    _otherwise ->
      Nothing

test_declarations_failing_redeclaration_different :: TestCase
test_declarations_failing_redeclaration_different =
  failingTestSimple "declarations/failing/redeclaration_different" $ \case
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

test_declarations_failing_tentative_definitions_linkage :: TestCase
test_declarations_failing_tentative_definitions_linkage =
  failingTestCustom "declarations/failing/tentative_definitions_linkage" [(), ()] $ \case
    TraceFrontend (FrontendClang (ClangDiagnostic x)) ->
      if "non-static declaration of" `Text.isInfixOf` diagnosticSpelling x
         then Just (Expected ())
         else Nothing
    TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
      Just Tolerated
    _otherwise ->
      Nothing

test_edge_cases_failing_unsupported_builtin :: TestCase
test_edge_cases_failing_unsupported_builtin =
  failingTestSimple "edge-cases/failing/unsupported_builtin" $ \case
    TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
      (AttachedParseMsg _ _ _
      (ParseUnsupportedType (UnsupportedBuiltin "__builtin_va_list")))))) ->
      Just $ Expected ()
    _otherwise ->
      Nothing

test_arrays_failing_array_res_1 :: TestCase
test_arrays_failing_array_res_1 =
  failingTestSimple "arrays/failing/array_res_1" $ \case
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

test_arrays_failing_array_res_2 :: TestCase
test_arrays_failing_array_res_2 =
  failingTestSimple "arrays/failing/array_res_2" $ \case
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

test_arrays_failing_array_res_3 :: TestCase
test_arrays_failing_array_res_3 =
  failingTestSimple "arrays/failing/array_res_3" $ \case
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

test_arrays_failing_array_res_4 :: TestCase
test_arrays_failing_array_res_4 =
  failingTestSimple "arrays/failing/array_res_4" $ \case
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

test_arrays_failing_array_res_5 :: TestCase
test_arrays_failing_array_res_5 =
  failingTestSimple "arrays/failing/array_res_5" $ \case
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

test_arrays_failing_array_res_6 :: TestCase
test_arrays_failing_array_res_6 =
  failingTestSimple "arrays/failing/array_res_6" $ \case
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

test_arrays_failing_array_res_7 :: TestCase
test_arrays_failing_array_res_7 =
  failingTestSimple "arrays/failing/array_res_7" $ \case
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

test_arrays_failing_array_res_8 :: TestCase
test_arrays_failing_array_res_8 =
  failingTestSimple "arrays/failing/array_res_8" $ \case
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

test_program_analysis_delay_traces :: TestCase
test_program_analysis_delay_traces =
  (defaultTest "program-analysis/delay_traces") {
    testOnFrontendConfig = \cfg -> cfg{
        frontendSelectPredicate =
          BOr
            (BIf (SelectDecl (DeclNameMatches "_function")))
            -- NOTE: Matching for name kind is not good practice, but we
            -- want to check if nested, but deselected declarations are
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

test_types_complex_complex_non_float_test :: TestCase
test_types_complex_complex_non_float_test =
  defaultTest "types/complex/complex_non_float_test"

test_documentation_doxygen_docs :: TestCase
test_documentation_doxygen_docs = (defaultTest "documentation/doxygen_docs") {
    testClangVersion = Just (>= (19, 0, 0))
  }

test_types_primitives_bool_c23 :: TestCase
test_types_primitives_bool_c23 = (defaultTest "types/primitives/bool_c23") {
    testClangVersion = Just (>= (15, 0, 0))
  }

test_functions_fun_attributes :: TestCase
test_functions_fun_attributes =
  (defaultTest "functions/fun_attributes") {
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
  }

test_functions_fun_attributes_conflict :: TestCase
test_functions_fun_attributes_conflict =
  (defaultTest "functions/fun_attributes_conflict") {
    testTracePredicate = customTracePredicate [] $ \case
      TraceFrontend (FrontendClang (ClangDiagnostic Diagnostic {diagnosticOption = Just "-Wno-ignored-attributes"})) ->
        Just Tolerated
      _otherwise ->
        Nothing
  }

test_globals_globals :: TestCase
test_globals_globals =
  let declsWithWarnings :: [Text]
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
       testTracePredicate = customTracePredicate' declsWithWarnings $ \case
         TraceFrontend (FrontendSelect (SelectParseSuccess
           (AttachedParseMsg i _ _ ParsePotentialDuplicateSymbol{}))) ->
           Just $ expectFromQualPrelimDeclId i
         TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
           (AttachedParseMsg i _ _ ParseUnexpectedAnonInExtern)))) ->
           Just $ expectFromQualPrelimDeclId i
         _otherwise ->
           Nothing
     }

test_edge_cases_iterator :: TestCase
test_edge_cases_iterator = (defaultTest "edge-cases/iterator") {
    testClangVersion     = Just (>= (15, 0, 0))
  , testOnBootConfig = \cfg -> cfg{
        bootClangArgsConfig = (bootClangArgsConfig cfg) {
            enableBlocks = True
          }
      }
  }

test_macros_macro_strings :: TestCase
test_macros_macro_strings = defaultTest "macros/macro_strings"

test_macros_macro_type_void :: TestCase
test_macros_macro_type_void = defaultTest "macros/macro_type_void"

test_types_structs_named_vs_anon :: TestCase
test_types_structs_named_vs_anon = (defaultTest "types/structs/named_vs_anon"){
    testClangVersion = Just (>= (19, 1, 0))
  }

test_program_analysis_program_slicing_simple :: TestCase
test_program_analysis_program_slicing_simple =
  (defaultTest "program-analysis/program_slicing_simple") {
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

test_program_analysis_selection_fail :: TestCase
test_program_analysis_selection_fail =
  testTraceCustom "program-analysis/selection_fail" [
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

test_program_analysis_selection_fail_variant_1 :: TestCase
test_program_analysis_selection_fail_variant_1 =
  (testVariant "program-analysis/selection_fail" "1.deselect_failed") {
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

test_program_analysis_selection_fail_variant_2 :: TestCase
test_program_analysis_selection_fail_variant_2 =
  (testVariant "program-analysis/selection_fail" "2.program_slicing"){
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

test_program_analysis_selection_fail_variant_3 :: TestCase
test_program_analysis_selection_fail_variant_3 =
  (testVariant "program-analysis/selection_fail" "3.select_ok"){
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

test_program_analysis_failing_selection_bad :: TestCase
test_program_analysis_failing_selection_bad =
  (defaultFailingTest "program-analysis/failing/selection_bad"){
    testTracePredicate = customTracePredicate ["size_t_select"] $ \case
      (TraceFrontend (FrontendSelect (TransitiveDependencyOfDeclarationUnavailable SelectionRoot (_, UnavailableNotSelected) _))) ->
        Just $ Expected "size_t_select"
      _other -> Nothing
  }

test_program_analysis_selection_foo :: TestCase
test_program_analysis_selection_foo =
  (defaultTest "program-analysis/selection_foo"){
    testTracePredicate = customTracePredicate' ["f"] $ \case
      (TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure m)))) ->
        Just $ expectFromQualPrelimDeclId m.declId
      _other -> Nothing
  }

test_program_analysis_program_slicing_selection :: TestCase
test_program_analysis_program_slicing_selection =
  (defaultTest "program-analysis/program_slicing_selection"){
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
  }

test_macros_reparse :: TestCase
test_macros_reparse = (defaultTest "macros/reparse") {
    testClangVersion   = Just (>= (15, 0, 0)) -- parse 'bool'
  , testTracePredicate = customTracePredicate [] $ \case
      -- We don't care about the trace messages in this test
      _anything ->
        Just Tolerated
  }

test_declarations_select_scoping :: TestCase
test_declarations_select_scoping =
  (defaultTest "declarations/select_scoping") {
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

test_edge_cases_failing_thread_local :: TestCase
test_edge_cases_failing_thread_local =
  (defaultFailingTest "edge-cases/failing/thread_local"){
    testClangVersion   = Just (>= (16, 0, 0))
  , testTracePredicate = singleTracePredicate $ \case
      TraceFrontend (FrontendSelect (SelectParseFailure (ParseFailure
        (AttachedParseMsg _ _ _ ParseUnsupportedTLS)))) ->
        Just $ Expected ()
      _otherwise ->
        Nothing
  }

test_edge_cases_failing_select_no_match :: TestCase
test_edge_cases_failing_select_no_match =
  (defaultFailingTest "edge-cases/failing/select_no_match") {
    testOnFrontendConfig = \cfg -> cfg {
        frontendSelectPredicate = BIf (SelectDecl (DeclNameMatches "this_pattern_will_never_match"))
      }
  , testTracePredicate = singleTracePredicate $ \case
      TraceFrontend (FrontendSelect SelectNoDeclarationsMatched) ->
        Just $ Expected ()
      _otherwise ->
        Nothing
  }

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = manualTestCases ++ [
      test_arrays_array
    , test_arrays_failing_array_res_1
    , test_arrays_failing_array_res_2
    , test_arrays_failing_array_res_3
    , test_arrays_failing_array_res_4
    , test_arrays_failing_array_res_5
    , test_arrays_failing_array_res_6
    , test_arrays_failing_array_res_7
    , test_arrays_failing_array_res_8
    , test_attributes_asm
    , test_attributes_attributes
    , test_attributes_type_attributes
    , test_attributes_visibility_attributes
    , test_binding_specs_bs_ext_target_any
    , test_binding_specs_bs_ext_target_mismatch
    , test_binding_specs_bs_pre_omit_type
    , test_binding_specs_bs_pre_rename_type
    , test_binding_specs_bs_pre_target_mismatch
    , test_declarations_declarations_required_for_scoping
    , test_declarations_definitions
    , test_declarations_failing_declaration_unselected_b
    , test_declarations_failing_redeclaration_different
    , test_declarations_failing_tentative_definitions_linkage
    , test_declarations_forward_declaration
    , test_declarations_opaque_declaration
    , test_declarations_redeclaration
    , test_declarations_redeclaration_identical
    , test_declarations_select_scoping
    , test_declarations_tentative_definitions
    , test_documentation_data_kind_pragma
    , test_documentation_doxygen_docs
    , test_edge_cases_adios
    , test_edge_cases_distilled_lib_1
    , test_edge_cases_failing_select_no_match
    , test_edge_cases_failing_thread_local
    , test_edge_cases_failing_unsupported_builtin
    , test_edge_cases_flam
    , test_edge_cases_headers
    , test_edge_cases_iterator
    , test_edge_cases_names
    , test_edge_cases_spec_examples
    , test_edge_cases_uses_utf8
    , test_functions_callbacks
    , test_functions_circular_dependency_fun
    , test_functions_decls_in_signature
    , test_functions_fun_attributes
    , test_functions_fun_attributes_conflict
    , test_functions_simple_func
    , test_functions_varargs
    , test_globals_globals
    , test_macros_macro_functions
    , test_macros_macro_in_fundecl
    , test_macros_macro_in_fundecl_vs_typedef
    , test_macros_macro_redefines_global
    , test_macros_macro_strings
    , test_macros_macro_type_void
    , test_macros_macro_typedef_scope
    , test_macros_macro_typedef_struct
    , test_macros_macro_types
    , test_macros_macros
    , test_macros_reparse
    , test_program_analysis_delay_traces
    , test_program_analysis_failing_selection_bad
    , test_program_analysis_program_slicing_selection
    , test_program_analysis_program_slicing_simple
    , test_program_analysis_selection_fail
    , test_program_analysis_selection_fail_variant_1
    , test_program_analysis_selection_fail_variant_2
    , test_program_analysis_selection_fail_variant_3
    , test_program_analysis_selection_foo
    , test_types_complex_complex_non_float_test
    , test_types_complex_hsb_complex_test
    , test_types_complex_vector_test
    , test_types_enums_enum_cpp_syntax
    , test_types_enums_enums
    , test_types_enums_nested_enums
    , test_types_failing_implicit_fields_struct
    , test_types_failing_implicit_fields_union
    , test_types_failing_long_double
    , test_types_nested_nested_types
    , test_types_primitives_bool
    , test_types_primitives_bool_c23
    , test_types_primitives_fixedwidth
    , test_types_primitives_primitive_types
    , test_types_qualifiers_type_qualifiers
    , test_types_special_parse_failure_long_double
    , test_types_structs_anonymous
    , test_types_structs_bitfields
    , test_types_structs_circular_dependency_struct
    , test_types_structs_named_vs_anon
    , test_types_structs_recursive_struct
    , test_types_structs_simple_structs
    , test_types_structs_struct_arg
    , test_types_structs_unnamed_struct
    , test_types_typedefs_typedef_analysis
    , test_types_typedefs_typedefs
    , test_types_typedefs_typedef_vs_macro
    , test_types_typedefs_typenames
    , test_types_unions_nested_unions
    , test_types_unions_unions
    ]

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
