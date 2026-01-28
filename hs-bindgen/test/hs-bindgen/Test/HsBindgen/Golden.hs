-- | Golden tests
module Test.HsBindgen.Golden (tests) where

import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Tasty

import Clang.LowLevel.Core
import Clang.Version

import HsBindgen.Backend.Category
import HsBindgen.Config.Internal (BackendConfig (..))
import HsBindgen.Frontend.Analysis.DeclIndex (Unusable (..))
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.MangleNames.Error (MangleNamesFailure (MangleNamesCollision))
import HsBindgen.Frontend.Pass.Parse.PrelimDeclId qualified as PrelimDeclId
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports ()
import HsBindgen.Language.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.TestCase.All qualified as Specs
import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.FailureBindgen qualified as FailureBindgen
import Test.HsBindgen.Golden.Check.FailureLibclang qualified as FailureLibclang
import Test.HsBindgen.Golden.Check.PP qualified as PP
import Test.HsBindgen.Golden.Check.TH qualified as TH
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources = testTreeFor testResources $
    TestCaseSection "Test.HsBindgen.Golden" [
        TestCases "default" testCases_default
      , TestCases "manual"  testCases_manual
      , TestCaseSection "bespoke" [
            TestCases "arrays"          testCases_bespoke_arrays
          , TestCases "attributes"      testCases_bespoke_attributes
          , TestCases "bindingSpecs"    testCases_bespoke_bindingSpecs
          , TestCases "declarations"    testCases_bespoke_declarations
          , TestCases "documentation"   testCases_bespoke_documentation
          , TestCases "edgeCases"       testCases_bespoke_edgeCases
          , TestCases "functions"       testCases_bespoke_functions
          , TestCases "globals"         testCases_bespoke_globals
          , TestCases "macros"          testCases_bespoke_macros
          , TestCases "programAnalysis" testCases_bespoke_programAnalysis
          , TestCases "types"           testCases_bespoke_types
          ]
      ]

{-------------------------------------------------------------------------------
  Construct tasty test tree
-------------------------------------------------------------------------------}

data TestCaseTree =
    TestCaseSection String [TestCaseTree]
  | TestCases String [TestCase]

testTreeFor :: IO TestResources -> TestCaseTree -> TestTree
testTreeFor testResources = goTree
  where
    goTree :: TestCaseTree -> TestTree
    goTree (TestCaseSection label sections) =
        testGroup label $ map goTree sections
    goTree (TestCases label cases) =
        testGroup label $ map goCase cases

    goCase :: TestCase -> TestTree
    goCase test
      | Just versionPred <- test.spec.clangVersion
      , case clangVersion of
          ClangVersion version  -> not (versionPred version)
          ClangVersionUnknown _ -> True
      = testGroup test.spec.name []

      | otherwise
      = case test.spec.outcome of
          Success ->
            withTestOutputDir test.outputDir $ testGroup test.spec.name [
                TH.check          testResources test
              , PP.check          testResources test
              , BindingSpec.check testResources test
              ]
          FailureBindgen ->
            FailureBindgen.check testResources test
          FailureLibclang ->
            FailureLibclang.check testResources test

    withTestOutputDir :: FilePath -> TestTree -> TestTree
    withTestOutputDir outputDir k =
        withResource
          (createDirectoryIfMissing True outputDir)
          (\_ -> pure ())
          (\_ -> k)

{-------------------------------------------------------------------------------
  Default tests (that is, test that do not require additional configuration)
-------------------------------------------------------------------------------}

testCases_default :: [TestCase]
testCases_default = map fromSpec Specs.testCaseSpecs_default

{-------------------------------------------------------------------------------
  Test cases that appear in the manual
-------------------------------------------------------------------------------}

testCases_manual :: [TestCase]
testCases_manual = [
      fromSpec $ Specs.testCaseSpecs_manual !! 0  -- manual/arrays
    , fromSpec $ Specs.testCaseSpecs_manual !! 1  -- manual/function_pointers
    , fromSpec $ Specs.testCaseSpecs_manual !! 2  -- manual/zero_copy
    , test_manual_globals
    ]

test_manual_globals :: TestCase
test_manual_globals =
    testTraceMulti "manual/globals" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDelayed name ParseUnexpectedAnonInExtern{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
        -- unexpected anon in extern
        "unusableAnon"
        -- potential duplicate symbols
      , "nonExternGlobalInt"
      ]

{-------------------------------------------------------------------------------
  Bespoke tests: arrays
-------------------------------------------------------------------------------}

testCases_bespoke_arrays :: [TestCase]
testCases_bespoke_arrays = [
      test_arrays_array
    , test_arrays_multi_dim
    , test_arrays_failing_array_res_1
    , test_arrays_failing_array_res_2
    , test_arrays_failing_array_res_3
    , test_arrays_failing_array_res_4
    , test_arrays_failing_array_res_5
    , test_arrays_failing_array_res_6
    , test_arrays_failing_array_res_7
    , test_arrays_failing_array_res_8
    ]

test_arrays_array :: TestCase
test_arrays_array =
    (fromSpec Specs.spec_arrays_array)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchDiagnosticOption "-Wno-extern-initializer" ->
              Just Tolerated
            MatchDiagnosticOption "-Wno-tentative-definition-array" ->
              Just Tolerated
            MatchDelayed name (MatchUnknownStorageClass CX_SC_Static) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
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

test_arrays_multi_dim :: TestCase
test_arrays_multi_dim = fromSpec Specs.spec_arrays_multi_dim

test_arrays_failing_array_res_1 :: TestCase
test_arrays_failing_array_res_1 =
    failingTestLibclangSimple "arrays/failing/array_res_1" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_2 :: TestCase
test_arrays_failing_array_res_2 =
    failingTestLibclangSimple "arrays/failing/array_res_2" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_3 :: TestCase
test_arrays_failing_array_res_3 =
    failingTestLibclangSimple "arrays/failing/array_res_3" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_4 :: TestCase
test_arrays_failing_array_res_4 =
    failingTestLibclangSimple "arrays/failing/array_res_4" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_5 :: TestCase
test_arrays_failing_array_res_5 =
    failingTestLibclangSimple "arrays/failing/array_res_5" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_6 :: TestCase
test_arrays_failing_array_res_6 =
    failingTestLibclangSimple "arrays/failing/array_res_6" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_7 :: TestCase
test_arrays_failing_array_res_7 =
    failingTestLibclangSimple "arrays/failing/array_res_7" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_arrays_failing_array_res_8 :: TestCase
test_arrays_failing_array_res_8 =
    failingTestLibclangSimple "arrays/failing/array_res_8" $ \case
      (matchDiagnosticSpelling "function cannot return array type" -> Just _diag) ->
        Just $ Expected ()
      TraceFrontend (FrontendClang _) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Bespoke test case: attributes
-------------------------------------------------------------------------------}

testCases_bespoke_attributes :: [TestCase]
testCases_bespoke_attributes = [
      test_attributes_asm
    , test_attributes_attributes
    , test_attributes_type_attributes
    , test_attributes_visibility_attributes
    ]

test_attributes_asm :: TestCase
test_attributes_asm = fromSpec Specs.spec_attributes_asm

test_attributes_attributes :: TestCase
test_attributes_attributes =
    testDiagnostic "attributes/attributes" $ \diag ->
      diagnosticCategoryText diag == "Nullability Issue"

test_attributes_type_attributes :: TestCase
test_attributes_type_attributes =
    testTraceSimple "attributes/type_attributes" $ \case
      MatchSelect _name SelectDeprecated{} ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_attributes_visibility_attributes :: TestCase
test_attributes_visibility_attributes =
    (fromSpec Specs.spec_attributes_visibility_attributes)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchDelayed name ParseNonPublicVisibility{} ->
              Just $ Expected name
            MatchDelayed name (MatchUnknownStorageClass CX_SC_Static) ->
              Just $ Expected name
            MatchDiagnosticOption "-Wno-extern-initializer" ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
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

{-------------------------------------------------------------------------------
  Bespoke tests: binding specs
-------------------------------------------------------------------------------}

testCases_bespoke_bindingSpecs :: [TestCase]
testCases_bespoke_bindingSpecs = [
      fromSpec Specs.spec_bindingSpecs_omit_type
      -- * Bugs / regression tests
    , test_bindingSpecs_macro_trans_dep_missing
      -- * Naming types
    , fromSpec Specs.spec_bindingSpecs_name_squash_both
    , fromSpec Specs.spec_bindingSpecs_name_squash_struct
    , fromSpec Specs.spec_bindingSpecs_name_squash_typedef
    , fromSpec Specs.spec_bindingSpecs_name_type
      -- * Representation: emptydata
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_enum
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_macro_type
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_opaque
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_struct
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_typedef
    , fromSpec Specs.spec_bindingSpecs_rep_emptydata_union
      -- * Function arguments with typedefs
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_array
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_array_known_size
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_enum
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_function
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_function_pointer
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_struct
    , fromSpec Specs.spec_bindingSpecs_fun_arg_typedef_union
      -- * Function arguments with macros
    , test_bindingSpecs_fun_arg_macro_array
    , test_bindingSpecs_fun_arg_macro_array_known_size
    , test_bindingSpecs_fun_arg_macro_enum
    , test_bindingSpecs_fun_arg_macro_function
    , test_bindingSpecs_fun_arg_macro_function_pointer
    , test_bindingSpecs_fun_arg_macro_struct
    , test_bindingSpecs_fun_arg_macro_union
    ]

-- | External binding specifications for macro types cause incorrect
-- TransitiveDependenciesMissing warnings
--
-- TODO: fix the 'TransitiveDependenciesMissing' warning. See issue #1513.
test_bindingSpecs_macro_trans_dep_missing :: TestCase
test_bindingSpecs_macro_trans_dep_missing =
    (fromSpec Specs.spec_bindingSpecs_macro_trans_dep_missing)
      { tracePredicate = multiTracePredicate ["foo" :: C.DeclName] (\case
            -- no macros should fail to parse
            MatchHandleMacros _ ->
              Just Unexpected
            -- TODO: Remove this case to see the 'TransitiveDependenciesMissing'
            -- warning. See issue #1513. Once the warning is fixed, this case
            -- can be removed indefinitely.
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just (Expected name)
            _otherwise ->
              Nothing
          )
      }

-- | Trace predicate for fun_arg_macro tests
noHandleMacrosTraces :: TracePredicate TraceMsg
noHandleMacrosTraces = multiTracePredicate ([] :: [String]) (\case
    -- no macros should fail to parse
    MatchHandleMacros _ ->
      Just Unexpected
    _otherwise ->
      Nothing
  )

test_bindingSpecs_fun_arg_macro_array :: TestCase
test_bindingSpecs_fun_arg_macro_array =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_array)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_array_known_size :: TestCase
test_bindingSpecs_fun_arg_macro_array_known_size =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_array_known_size)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_enum :: TestCase
test_bindingSpecs_fun_arg_macro_enum =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_enum)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_function :: TestCase
test_bindingSpecs_fun_arg_macro_function =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_function)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_function_pointer :: TestCase
test_bindingSpecs_fun_arg_macro_function_pointer =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_function_pointer)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_struct :: TestCase
test_bindingSpecs_fun_arg_macro_struct =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_struct)
      { tracePredicate = noHandleMacrosTraces }

test_bindingSpecs_fun_arg_macro_union :: TestCase
test_bindingSpecs_fun_arg_macro_union =
    (fromSpec Specs.spec_bindingSpecs_fun_arg_macro_union)
      { tracePredicate = noHandleMacrosTraces }

{-------------------------------------------------------------------------------
  Bespoke tests: declarations
-------------------------------------------------------------------------------}

testCases_bespoke_declarations :: [TestCase]
testCases_bespoke_declarations = [
      test_declarations_declaration_unselected_b
    , test_declarations_definitions
    , test_declarations_failing_tentative_definitions_linkage
    , test_declarations_name_collision
    , test_declarations_redeclaration
    , test_declarations_redeclaration_different
    , test_declarations_select_scoping
    , test_declarations_tentative_definitions
    ]

test_declarations_declaration_unselected_b :: TestCase
test_declarations_declaration_unselected_b =
    testTraceMulti "declarations/declaration_unselected_b" declsWithMsgs $ \case
      MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f"]

test_declarations_definitions :: TestCase
test_declarations_definitions =
    testTraceMulti "declarations/definitions" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["foo", "n"]

test_declarations_failing_tentative_definitions_linkage :: TestCase
test_declarations_failing_tentative_definitions_linkage =
    failingTestLibclangMulti "declarations/failing/tentative_definitions_linkage" [(), ()] $ \case
      (matchDiagnosticSpelling "non-static declaration of" -> Just _diag) ->
        Just $ Expected ()
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

-- This tests https://github.com/well-typed/hs-bindgen/issues/1373.
test_declarations_name_collision :: TestCase
test_declarations_name_collision =
    testTraceMulti "declarations/name_collision" declsWithMsgs $ \case
      MatchSelect name (SelectMangleNamesFailure MangleNamesCollision{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["union y", "union Y"]

test_declarations_redeclaration :: TestCase
test_declarations_redeclaration =
    testTraceMulti "declarations/redeclaration" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDelayed name (MatchUnknownStorageClass CX_SC_Static) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["x", "n"]

test_declarations_redeclaration_different :: TestCase
test_declarations_redeclaration_different =
    testTraceSimple "declarations/redeclaration_different" $ \case
      MatchSelect _name SelectConflict{} ->
        Just $ Expected ()
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_declarations_select_scoping :: TestCase
test_declarations_select_scoping =
    (fromSpec Specs.spec_declarations_select_scoping)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            MatchSelect name (MatchTransMissing [MatchTransUnusable UnusableParseNotAttempted{}]) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "ParsedAndSelected2"
        , "ParsedAndSelected3"
        , "struct ParsedUnselectable"
        , "ParsedAndSelected4"
        , "ParsedAndSelected5"
        ]

test_declarations_tentative_definitions :: TestCase
test_declarations_tentative_definitions =
    testTraceMulti "declarations/tentative_definitions" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDelayed name (MatchUnknownStorageClass CX_SC_Static) ->
        Just $ Expected name
      MatchDiagnosticOption "-Wno-extern-initializer" ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["i1", "i2", "i3"]

{-------------------------------------------------------------------------------
  Bespoke tests: documentation
-------------------------------------------------------------------------------}

testCases_bespoke_documentation :: [TestCase]
testCases_bespoke_documentation = [
      fromSpec Specs.spec_documentation_doxygen_docs
    ]

{-------------------------------------------------------------------------------
  Bespoke tests: edge cases
-------------------------------------------------------------------------------}

testCases_bespoke_edgeCases :: [TestCase]
testCases_bespoke_edgeCases = [
      test_edgeCases_clang_generated_collision
    , test_edgeCases_duplicate
    , test_edgeCases_headers
    , test_edgeCases_iterator
    , test_edgeCases_ordinary_anon
    , test_edgeCases_select_no_match
    , test_edgeCases_thread_local
    , test_edgeCases_unsupported_builtin
    ]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1389>
-- For now we report a collision between the two @struct foo@
-- declarations, but no collision between those and the typedef.
test_edgeCases_clang_generated_collision :: TestCase
test_edgeCases_clang_generated_collision =
    (fromSpec Specs.spec_edgeCases_clang_generated_collision)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected name
            MatchSelect _ (MatchTransMissing [MatchTransUnusable UnusableConflict{}]) ->
              Just Tolerated
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct foo"]

test_edgeCases_duplicate :: TestCase
test_edgeCases_duplicate =
    (fromSpec Specs.spec_edgeCases_duplicate)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name SelectConflict{} ->
              Just $ Expected (name, "conflict")
            MatchSelect name (MatchTransMissing [MatchTransUnusable UnusableConflict{}]) ->
              Just $ Expected (name, "transitive conflict")
            _otherwise ->
               Nothing
          )
      }
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("duplicate", "conflict")
        , ("function", "transitive conflict")
        ]

test_edgeCases_headers :: TestCase
test_edgeCases_headers =
    testTraceSimple "edge-cases/headers" $ \case
      MatchNoDeclarations ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_edgeCases_iterator :: TestCase
test_edgeCases_iterator = fromSpec Specs.spec_edgeCases_iterator

test_edgeCases_ordinary_anon :: TestCase
test_edgeCases_ordinary_anon = fromSpec Specs.spec_edgeCases_ordinary_anon

test_edgeCases_select_no_match :: TestCase
test_edgeCases_select_no_match =
    (fromSpec Specs.spec_edgeCases_select_no_match)
      { tracePredicate = singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )
      }

test_edgeCases_thread_local :: TestCase
test_edgeCases_thread_local =
    (fromSpec Specs.spec_edgeCases_thread_local)
      { tracePredicate = singleTracePredicate (\case
            MatchDelayed _name ParseUnsupportedTLS ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )
      }

test_edgeCases_unsupported_builtin :: TestCase
test_edgeCases_unsupported_builtin =
    testTraceMulti "edge-cases/unsupported_builtin" declsWithMsgs $ \case
      MatchDelayed name (ParseUnsupportedType (UnsupportedBuiltin "__builtin_va_list")) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["va_list"]

{-------------------------------------------------------------------------------
  Functions
-------------------------------------------------------------------------------}

testCases_bespoke_functions :: [TestCase]
testCases_bespoke_functions = [
      test_functions_decls_in_signature
    , test_functions_fun_attributes
    , test_functions_fun_attributes_conflict
    , test_functions_simple_func_rename
    , test_functions_varargs
    ]

test_functions_decls_in_signature :: TestCase
test_functions_decls_in_signature =
    testTraceMulti "functions/decls_in_signature" declsWithMsgs $ \case
      MatchDelayed name ParseUnexpectedAnonInSignature{} ->
        Just $ Expected name
      MatchDiagnosticOption _diag ->
        Just $ Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f3", "f4", "f5"]

test_functions_fun_attributes :: TestCase
test_functions_fun_attributes =
    (fromSpec Specs.spec_functions_fun_attributes)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchDelayed name (ParseUnsupportedType UnsupportedVariadicFunction) ->
              Just $ Expected name
            MatchDelayed name ParseNonPublicVisibility ->
              Just $ Expected name
            MatchDelayed name ParsePotentialDuplicateSymbol{} ->
              Just $ Expected name
            MatchSelect name SelectDeprecated{} ->
              Just $ Expected name
            MatchSelect name (SelectParseNotAttempted DeclarationUnavailable) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "my_printf"
        , "i"
        , "f3"
        , "old_fn_deprecated"
        , "old_fn_unavailable"
        ]

test_functions_fun_attributes_conflict :: TestCase
test_functions_fun_attributes_conflict =
    testTraceMulti "functions/fun_attributes_conflict" declsWithMsgs $ \case
      MatchDiagnosticOption "-Wno-ignored-attributes" ->
        Just Tolerated
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = []

test_functions_simple_func_rename :: TestCase
test_functions_simple_func_rename =
    (testVariant "functions/simple_func" "1.rename")
      { onBackend = \cfg -> cfg { categoryChoice = ByCategory {
            cType = IncludeTypeCategory
          , cSafe = ExcludeCategory
          , cUnsafe = ExcludeCategory
          , cFunPtr = IncludeTermCategory $ RenameTerm $ \t -> t <> "_random_user_specified_suffix"
          , cGlobal = ExcludeCategory
          }}
      }

test_functions_varargs :: TestCase
test_functions_varargs =
    testTraceMulti "functions/varargs" declsWithMsgs $ \case
      MatchDelayed name (ParseUnsupportedType UnsupportedVariadicFunction) ->
        Just $ Expected name
      MatchDelayed name (
          ParseUnsupportedType (
            UnsupportedUnderlyingType
              (PrelimDeclId.Named (C.DeclName "va_list" C.NameKindOrdinary))
              (UnsupportedBuiltin "__builtin_va_list")
          )
        ) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f", "g"]

{-------------------------------------------------------------------------------
  Bespoke tests: globals
-------------------------------------------------------------------------------}

testCases_bespoke_globals :: [TestCase]
testCases_bespoke_globals = [
      test_globals_globals
    ]

test_globals_globals :: TestCase
test_globals_globals =
    testTraceMulti "globals/globals" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      MatchDelayed name ParseUnexpectedAnonInExtern{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
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
        ]

{-------------------------------------------------------------------------------
  Bespoke tests: macros
-------------------------------------------------------------------------------}

testCases_bespoke_macros :: [TestCase]
testCases_bespoke_macros = [
      test_macros_macro_in_fundecl
    , test_macros_macro_in_fundecl_vs_typedef
    , test_macros_macro_redefines_global
    , test_macros_reparse
    ]

test_macros_macro_in_fundecl :: TestCase
test_macros_macro_in_fundecl =
    testTraceMulti "macros/macro_in_fundecl" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          -- Duplicate symbols
          "bar1", "bar2", "bar3", "bar4"
        , "baz1", "baz2", "baz3"
        , "foo1", "foo2", "foo3"
        , "quux"
        , "wam"
        ]

test_macros_macro_in_fundecl_vs_typedef :: TestCase
test_macros_macro_in_fundecl_vs_typedef =
    testTraceMulti "macros/macro_in_fundecl_vs_typedef" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["quux1", "quux2", "wam1", "wam2"]

test_macros_macro_redefines_global :: TestCase
test_macros_macro_redefines_global =
    testTraceMulti "macros/macro_redefines_global" declsWithMsgs $ \case
      MatchSelect name SelectConflict{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["stdin", "stdout", "stderr"]

test_macros_reparse :: TestCase
test_macros_reparse =
    (fromSpec Specs.spec_macros_reparse)
      { tracePredicate = tolerateAll
      }

{-------------------------------------------------------------------------------
  Bespoke tests: program analysis
-------------------------------------------------------------------------------}

testCases_bespoke_programAnalysis :: [TestCase]
testCases_bespoke_programAnalysis = [
      test_programAnalysis_delay_traces
    , test_programAnalysis_program_slicing_selection
    , test_programAnalysis_program_slicing_simple
    , test_programAnalysis_selection_bad
    , test_programAnalysis_selection_fail
    , test_programAnalysis_selection_fail_variant_1
    , test_programAnalysis_selection_fail_variant_2
    , test_programAnalysis_selection_fail_variant_3
    , test_programAnalysis_selection_foo
    , test_programAnalysis_selection_matches_c_names_1
    , test_programAnalysis_selection_matches_c_names_2
    , test_programAnalysis_selection_merge_traces
    , test_programAnalysis_selection_omit_external_a
    , test_programAnalysis_selection_omit_external_b
    , test_programAnalysis_selection_omit_prescriptive
    , test_programAnalysis_selection_squash
    , test_programAnalysis_typedef_analysis
    ]

test_programAnalysis_delay_traces :: TestCase
test_programAnalysis_delay_traces =
    (fromSpec Specs.spec_programAnalysis_delay_traces)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchDelayed name (ParseUnsupportedType UnsupportedLongDouble) ->
              Just $ Expected name
            MatchDelayed name (ParseUnsupportedType UnsupportedVariadicFunction) ->
              Just $ Expected name
            _otherwise ->
               Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "long_double_function"
        , "var_arg_function"
        , "struct long_double_s"
        , "struct nested_long_double_s"
        ]

test_programAnalysis_program_slicing_selection :: TestCase
test_programAnalysis_program_slicing_selection =
    (fromSpec Specs.spec_programAnalysis_program_slicing_selection)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected (name, "root")
            MatchSelect name (SelectStatusInfo (Selected TransitiveDependency)) ->
              Just $ Expected (name, "dependency")
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("struct FileOperationRecord" , "root")
        , ("read_file_chunk"            , "root")
        , ("enum FileOperationStatus"   , "dependency")
        ]

-- Check that program slicing generates bindings for uint32_t and uint64_t if we
-- only provide external binding specifications for uint64_t.
test_programAnalysis_program_slicing_simple :: TestCase
test_programAnalysis_program_slicing_simple =
    (fromSpec Specs.spec_programAnalysis_program_slicing_simple)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected (name, "root")
            MatchSelect name (SelectStatusInfo (Selected TransitiveDependency)) ->
              Just $ Expected (name, "dependency")
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
          ("struct foo" , "root")
        , ("bar"        , "root")
        , ("uint32_t"   , "dependency")
        ]

test_programAnalysis_selection_bad :: TestCase
test_programAnalysis_selection_bad =
    testTraceMulti "program-analysis/selection_bad" declsWithMsgs $ \case
      MatchSelect name MatchTransMissing{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    -- @f@ depends on user-defined @size_t@, which is not selected
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f"]

test_programAnalysis_selection_fail :: TestCase
test_programAnalysis_selection_fail =
    testTraceMulti "program-analysis/selection_fail" declsWithMsgs $ \case
      MatchSelect name SelectParseFailure{} ->
        Just $ Expected name
      MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
        Just $ Expected name
      MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct Fail"
        , "struct Fail"
        , "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_1 :: TestCase
test_programAnalysis_selection_fail_variant_1 =
    (fromSpec Specs.spec_programAnalysis_selection_fail_variant_1)
      { outputDir = "fixtures" </> "program-analysis/selection_fail.1.deselect_failed"
      , tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just $ Expected name
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_2 :: TestCase
test_programAnalysis_selection_fail_variant_2 =
    (fromSpec Specs.spec_programAnalysis_selection_fail_variant_2)
      { outputDir = "fixtures" </> "program-analysis/selection_fail.2.program_slicing"
      , tracePredicate = multiTracePredicate declsWithMsgs (\case
           MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
             Just $ Expected name
           MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
             Just $ Expected name
           _otherwise ->
             Nothing
         )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct DependOnFailByValue"
        , "struct DependOnFailByReference"
        , "struct OkBefore"
        , "struct OkAfter"
        ]

test_programAnalysis_selection_fail_variant_3 :: TestCase
test_programAnalysis_selection_fail_variant_3 =
    (fromSpec Specs.spec_programAnalysis_selection_fail_variant_3)
      { outputDir = "fixtures" </> "program-analysis/selection_fail.3.select_ok"
      , tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (SelectStatusInfo (Selected SelectionRoot)) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct OkBefore"]

test_programAnalysis_selection_foo :: TestCase
test_programAnalysis_selection_foo =
    testTraceMulti "program-analysis/selection_foo" declsWithMsgs $ \case
      MatchSelect name SelectParseFailure{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["f"]

test_programAnalysis_selection_matches_c_names_1 :: TestCase
test_programAnalysis_selection_matches_c_names_1 =
    fromSpec Specs.spec_programAnalysis_selection_matches_c_names_1

test_programAnalysis_selection_matches_c_names_2 :: TestCase
test_programAnalysis_selection_matches_c_names_2 =
    (fromSpec Specs.spec_programAnalysis_selection_matches_c_names_2)
      { tracePredicate = singleTracePredicate (\case
            MatchNoDeclarations ->
              Just $ Expected ()
            _otherwise ->
              Nothing
          )
      }

test_programAnalysis_selection_merge_traces :: TestCase
test_programAnalysis_selection_merge_traces =
    (fromSpec Specs.spec_programAnalysis_selection_merge_traces)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [_, _]) -> Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["dependsOnXAndY"]

test_programAnalysis_selection_omit_external_a :: TestCase
test_programAnalysis_selection_omit_external_a =
    (fromSpec Specs.spec_programAnalysis_selection_omit_external_a)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchResolveBindingSpecs (ResolveBindingSpecsOmittedType declId) ->
              Just $ Expected declId.name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct Omitted"]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1361>
-- We should warn when we create bindings for a declaration omitted by an
-- /external/ binding specification.
test_programAnalysis_selection_omit_external_b :: TestCase
test_programAnalysis_selection_omit_external_b =
    fromSpec Specs.spec_programAnalysis_selection_omit_external_b

test_programAnalysis_selection_omit_prescriptive :: TestCase
test_programAnalysis_selection_omit_prescriptive =
    (fromSpec Specs.spec_programAnalysis_selection_omit_prescriptive)
      { tracePredicate = multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing [MatchTransUnusable _unusable]) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
      }
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
          "struct DirectlyDependsOnOmitted"
        , "struct IndirectlyDependsOnOmitted"
        ]

test_programAnalysis_selection_squash :: TestCase
test_programAnalysis_selection_squash =
    testTraceMulti "program-analysis/selection_squash_typedef" declsWithMsgs $ \case
      MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["typedef_to_struct_a"]

test_programAnalysis_typedef_analysis :: TestCase
test_programAnalysis_typedef_analysis =
    testTraceMulti "program-analysis/typedef_analysis" declsWithMsgs $ \case
      MatchSelect name SelectMangleNamesSquashed{} ->
        Just $ Expected (name, Nothing)
      MatchMangle name (MangleNamesAssignedName new) ->
        Just $ Expected (name, Just new)
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, Maybe Hs.Identifier)]
    declsWithMsgs = [
          ("struct struct1"  , Just "Struct1_t")
        , ("struct1_t"       , Nothing)
        , ("struct struct2"  , Just "Struct2_t")
        , ("struct2_t"       , Nothing)
        , ("struct struct3"  , Just "Struct3_t")
        , ("struct3_t"       , Nothing)
        , ("struct struct4"  , Just "Struct4_t")
        , ("struct4_t"       , Nothing)
        , ("struct struct6"  , Just "Struct6_Aux")
        , ("struct8"         , Nothing)
        , ("struct9"         , Nothing)
        , ("struct struct10" , Just "Struct10_t")
        , ("struct10_t"      , Nothing)
        , ("struct struct11" , Just "Struct11_t")
        , ("struct11_t"      , Nothing)
        , ("struct struct12" , Just "Struct12_t")
        , ("struct12_t"      , Nothing)
        ]


{-------------------------------------------------------------------------------
  Bespoke tests: types
-------------------------------------------------------------------------------}

testCases_bespoke_types :: [TestCase]
testCases_bespoke_types = [
      test_types_implicit_fields_struct
    , test_types_implicit_fields_union
    , test_types_long_double
    , test_types_primitives_bool_c23
    , test_types_special_parse_failure_long_double
    , test_types_structs_named_vs_anon
    , test_types_structs_unnamed_struct
    , test_types_typedefs_typedefs
    , test_types_typedefs_typenames
    ]

test_types_implicit_fields_struct :: TestCase
test_types_implicit_fields_struct =
    testTraceSimple "types/structs/implicit_fields_struct" $ \case
      MatchDelayed _name ParseUnsupportedImplicitFields{} ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_types_implicit_fields_union :: TestCase
test_types_implicit_fields_union =
    testTraceSimple "types/unions/implicit_fields_union" $ \case
      MatchDelayed _name ParseUnsupportedImplicitFields{} ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_types_long_double :: TestCase
test_types_long_double =
    testTraceSimple "types/special/long_double" $ \case
      MatchDelayed _name (ParseUnsupportedType UnsupportedLongDouble) ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_types_primitives_bool_c23 :: TestCase
test_types_primitives_bool_c23 = fromSpec Specs.spec_types_primitives_bool_c23

test_types_special_parse_failure_long_double :: TestCase
test_types_special_parse_failure_long_double =
    testTraceMulti "types/special/parse_failure_long_double" declsWithMsgs $ \case
      MatchDelayed name (ParseUnsupportedType UnsupportedLongDouble) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["fun1", "struct struct1"]

test_types_structs_named_vs_anon :: TestCase
test_types_structs_named_vs_anon = fromSpec Specs.spec_types_structs_named_vs_anon

test_types_structs_unnamed_struct :: TestCase
test_types_structs_unnamed_struct =
    testTraceSimple "types/structs/unnamed-struct" $ \case
      MatchDiagnosticCategory "Semantic Issue" ->
        Just $ Expected ()
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_types_typedefs_typedefs :: TestCase
test_types_typedefs_typedefs =
    testTraceMulti "types/typedefs/typedefs" declsWithMsgs $ \case
      MatchDelayed name ParseFunctionOfTypeTypedef ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["foo"]

-- This tests https://github.com/well-typed/hs-bindgen/issues/1389.
test_types_typedefs_typenames :: TestCase
test_types_typedefs_typenames =
    testTraceMulti "types/typedefs/typenames" declsWithMsgs $ \case
      MatchSelect name (SelectMangleNamesFailure MangleNamesCollision{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["enum foo", "foo"]
