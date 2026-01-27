{-# LANGUAGE CPP #-}

-- | All test case specifications
--
-- This module exports all test case specifications that are used by both
-- golden tests and TH fixture compilation tests. The test cases are organized
-- into sections that mirror the golden test structure.
--
module Test.Common.HsBindgen.TestCase.All (
    allTestCaseSpecs
    -- * Test case sections
  , testCaseSpecs_default
  , testCaseSpecs_manual
  , testCaseSpecs_bespoke_arrays
  , testCaseSpecs_bespoke_attributes
  , testCaseSpecs_bespoke_bindingSpecs
  , testCaseSpecs_bespoke_declarations
  , testCaseSpecs_bespoke_documentation
  , testCaseSpecs_bespoke_edgeCases
  , testCaseSpecs_bespoke_functions
  , testCaseSpecs_bespoke_globals
  , testCaseSpecs_bespoke_macros
  , testCaseSpecs_bespoke_programAnalysis
  , testCaseSpecs_bespoke_types
    -- * Individual specs (for golden tests that need to customize)
    -- ** Arrays
  , spec_arrays_array
    -- ** Attributes
  , spec_attributes_asm
  , spec_attributes_visibility_attributes
    -- ** Binding specs
  , spec_bindingSpecs_bs_pre_omit_type
  , spec_bindingSpecs_bs_pre_name_squash_both
  , spec_bindingSpecs_bs_pre_name_squash_struct
  , spec_bindingSpecs_bs_pre_name_squash_typedef
  , spec_bindingSpecs_bs_pre_name_type
  , spec_bindingSpecs_macro_trans_dep_missing
  , spec_bindingSpecs_fun_arg_typedef_array
  , spec_bindingSpecs_fun_arg_typedef_array_known_size
  , spec_bindingSpecs_fun_arg_typedef_enum
  , spec_bindingSpecs_fun_arg_typedef_function
  , spec_bindingSpecs_fun_arg_typedef_function_pointer
  , spec_bindingSpecs_fun_arg_typedef_struct
  , spec_bindingSpecs_fun_arg_typedef_union
  , spec_bindingSpecs_fun_arg_macro_array
  , spec_bindingSpecs_fun_arg_macro_array_known_size
  , spec_bindingSpecs_fun_arg_macro_enum
  , spec_bindingSpecs_fun_arg_macro_function
  , spec_bindingSpecs_fun_arg_macro_function_pointer
  , spec_bindingSpecs_fun_arg_macro_struct
  , spec_bindingSpecs_fun_arg_macro_union
    -- ** Declarations
  , spec_declarations_select_scoping
    -- ** Documentation
  , spec_documentation_doxygen_docs
    -- ** Edge cases
  , spec_edgeCases_clang_generated_collision
  , spec_edgeCases_duplicate
  , spec_edgeCases_iterator
  , spec_edgeCases_ordinary_anon
  , spec_edgeCases_select_no_match
  , spec_edgeCases_thread_local
    -- ** Functions
  , spec_functions_fun_attributes
    -- ** Macros
  , spec_macros_reparse
    -- ** Program analysis
  , spec_programAnalysis_delay_traces
  , spec_programAnalysis_program_slicing_selection
  , spec_programAnalysis_program_slicing_simple
  , spec_programAnalysis_selection_fail_variant_1
  , spec_programAnalysis_selection_fail_variant_2
  , spec_programAnalysis_selection_fail_variant_3
  , spec_programAnalysis_selection_merge_traces
  , spec_programAnalysis_selection_omit_external_a
  , spec_programAnalysis_selection_omit_external_b
  , spec_programAnalysis_selection_omit_prescriptive
    -- ** Types
  , spec_types_complex_hsb_complex_test
  , spec_types_complex_non_float_test
  , spec_types_primitives_bool_c23
  , spec_types_structs_named_vs_anon
  ) where

import System.FilePath ((<.>), (</>))

import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports

import Test.Common.HsBindgen.TestCase.Spec

{-------------------------------------------------------------------------------
  All test case specifications
-------------------------------------------------------------------------------}

-- | All test case specifications
--
-- This is the complete list of all test case specifications from the golden
-- tests, suitable for use by both golden tests and TH fixture compilation.
--
allTestCaseSpecs :: [TestCaseSpec]
allTestCaseSpecs = concat [
      testCaseSpecs_default
    , testCaseSpecs_manual
    , testCaseSpecs_bespoke_arrays
    , testCaseSpecs_bespoke_attributes
    , testCaseSpecs_bespoke_bindingSpecs
    , testCaseSpecs_bespoke_declarations
    , testCaseSpecs_bespoke_documentation
    , testCaseSpecs_bespoke_edgeCases
    , testCaseSpecs_bespoke_functions
    , testCaseSpecs_bespoke_globals
    , testCaseSpecs_bespoke_macros
    , testCaseSpecs_bespoke_programAnalysis
    , testCaseSpecs_bespoke_types
    ]

{-------------------------------------------------------------------------------
  Default tests (that is, tests that do not require additional configuration)
-------------------------------------------------------------------------------}

testCaseSpecs_default :: [TestCaseSpec]
testCaseSpecs_default = [
      defaultSpec "declarations/declarations_required_for_scoping"
    , defaultSpec "declarations/forward_declaration"
    , defaultSpec "declarations/opaque_declaration"
    , defaultSpec "declarations/redeclaration_identical"
    , defaultSpec "documentation/data_kind_pragma"
    , defaultSpec "edge-cases/adios"
    , defaultSpec "edge-cases/anon_multiple_fields"
    , defaultSpec "edge-cases/anon_multiple_typedefs"
    , defaultSpec "edge-cases/distilled_lib_1"
    , defaultSpec "edge-cases/enum_as_array_size"
    , defaultSpec "edge-cases/flam"
    , defaultSpec "edge-cases/flam_functions"
    , defaultSpec "edge-cases/names"
    , defaultSpec "edge-cases/spec_examples"
    , defaultSpec "edge-cases/typedef_bitfield"
    , defaultSpec "edge-cases/typedef_void"
    , defaultSpec "edge-cases/uses_utf8"
    , defaultSpec "functions/callbacks"
    , defaultSpec "functions/circular_dependency_fun"
    , defaultSpec "functions/heap_types/struct_const_member"
    , defaultSpec "functions/heap_types/struct_const_typedef"
    , defaultSpec "functions/heap_types/struct_const"
    , defaultSpec "functions/heap_types/struct"
    , defaultSpec "functions/heap_types/union_const_member"
    , defaultSpec "functions/heap_types/union_const_typedef"
    , defaultSpec "functions/heap_types/union_const"
    , defaultSpec "functions/heap_types/union"
    , defaultSpec "functions/simple_func"
    , defaultSpec "macros/issue_890"
    , defaultSpec "macros/macro_functions"
    , defaultSpec "macros/macro_strings"
    , defaultSpec "macros/macro_type_void"
    , defaultSpec "macros/macro_typedef_scope"
    , defaultSpec "macros/macro_typedef_struct"
    , defaultSpec "macros/macro_types"
    , defaultSpec "macros/macros"
    , spec_types_complex_non_float_test
    , spec_types_complex_hsb_complex_test
    , defaultSpec "types/complex/vector_test"
    , defaultSpec "types/enums/anon_enum_toplevel"
    , defaultSpec "types/enums/enum_cpp_syntax"
    , defaultSpec "types/enums/enums"
    , defaultSpec "types/enums/nested_enums"
    , defaultSpec "types/nested/nested_types"
    , defaultSpec "types/primitives/bool"
    , defaultSpec "types/primitives/fixedwidth"
    , defaultSpec "types/primitives/primitive_types"
    , defaultSpec "types/qualifiers/type_qualifiers"
    , defaultSpec "types/qualifiers/const_typedefs"
    , defaultSpec "types/structs/anonymous"
      -- Note: On Windows, this test is skipped in TH fixture compilation because
      -- the overflow64 struct contains 'long x : 33' which is invalid on Windows
      -- (or at least the CI Windows machine) where 'long' is 32-bit (LLP64 model).
      --
    , defaultSpec "types/structs/bitfields"
    , defaultSpec "types/structs/circular_dependency_struct"
    , defaultSpec "types/structs/recursive_struct"
    , defaultSpec "types/structs/simple_structs"
    , defaultSpec "types/structs/struct_arg"
    , defaultSpec "types/typedefs/multi_level_function_pointer"
    , defaultSpec "types/typedefs/typedef_vs_macro"
    , defaultSpec "types/unions/nested_unions"
    , defaultSpec "types/unions/unions"
    ]

{-------------------------------------------------------------------------------
  Test cases that appear in the manual
-------------------------------------------------------------------------------}

testCaseSpecs_manual :: [TestCaseSpec]
testCaseSpecs_manual = [
      defaultSpec "manual/arrays"
    , defaultSpec "manual/function_pointers"
    , defaultSpec "manual/zero_copy"
    , defaultSpec "manual/globals"
    ]

{-------------------------------------------------------------------------------
  Bespoke tests: arrays
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_arrays :: [TestCaseSpec]
testCaseSpecs_bespoke_arrays = [
      spec_arrays_array
    , spec_arrays_failing_array_res_1
    , spec_arrays_failing_array_res_2
    , spec_arrays_failing_array_res_3
    , spec_arrays_failing_array_res_4
    , spec_arrays_failing_array_res_5
    , spec_arrays_failing_array_res_6
    , spec_arrays_failing_array_res_7
    , spec_arrays_failing_array_res_8
    ]

spec_arrays_array :: TestCaseSpec
spec_arrays_array =
    (defaultSpec "arrays/array")
      { clangVersion = Just (>= (19, 0, 0))
      }

spec_arrays_failing_array_res_1 :: TestCaseSpec
spec_arrays_failing_array_res_1 =
    (defaultSpec "arrays/failing/array_res_1")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_2 :: TestCaseSpec
spec_arrays_failing_array_res_2 =
    (defaultSpec "arrays/failing/array_res_2")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_3 :: TestCaseSpec
spec_arrays_failing_array_res_3 =
    (defaultSpec "arrays/failing/array_res_3")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_4 :: TestCaseSpec
spec_arrays_failing_array_res_4 =
    (defaultSpec "arrays/failing/array_res_4")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_5 :: TestCaseSpec
spec_arrays_failing_array_res_5 =
    (defaultSpec "arrays/failing/array_res_5")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_6 :: TestCaseSpec
spec_arrays_failing_array_res_6 =
    (defaultSpec "arrays/failing/array_res_6")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_7 :: TestCaseSpec
spec_arrays_failing_array_res_7 =
    (defaultSpec "arrays/failing/array_res_7")
      { outcome = FailureLibclang
      }

spec_arrays_failing_array_res_8 :: TestCaseSpec
spec_arrays_failing_array_res_8 =
    (defaultSpec "arrays/failing/array_res_8")
      { outcome = FailureLibclang
      }

{-------------------------------------------------------------------------------
  Bespoke tests: attributes
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_attributes :: [TestCaseSpec]
testCaseSpecs_bespoke_attributes = [
      spec_attributes_asm
    , spec_attributes_attributes
    , spec_attributes_type_attributes
    , spec_attributes_visibility_attributes
    ]

spec_attributes_asm :: TestCaseSpec
spec_attributes_asm =
    (defaultSpec "attributes/asm")
      { clangVersion = Just (>= (18, 0, 0))
      , onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { argsBefore = ["-std=gnu2x"] } }
      }

spec_attributes_attributes :: TestCaseSpec
spec_attributes_attributes = defaultSpec "attributes/attributes"

spec_attributes_type_attributes :: TestCaseSpec
spec_attributes_type_attributes = defaultSpec "attributes/type_attributes"

spec_attributes_visibility_attributes :: TestCaseSpec
spec_attributes_visibility_attributes =
    (defaultSpec "attributes/visibility_attributes")
      { onFrontend = #selectPredicate .~
            BAnd
              (BIf (SelectHeader FromMainHeaders))
              (BNot (BIf (SelectDecl DeclDeprecated)))
      }

{-------------------------------------------------------------------------------
  Bespoke tests: binding specs
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_bindingSpecs :: [TestCaseSpec]
testCaseSpecs_bespoke_bindingSpecs = [
      spec_bindingSpecs_bs_pre_omit_type
    , spec_bindingSpecs_bs_pre_name_squash_both
    , spec_bindingSpecs_bs_pre_name_squash_struct
    , spec_bindingSpecs_bs_pre_name_squash_typedef
    , spec_bindingSpecs_bs_pre_name_type
    , spec_bindingSpecs_macro_trans_dep_missing
      -- * Function arguments with typedefs
    , spec_bindingSpecs_fun_arg_typedef_array
    , spec_bindingSpecs_fun_arg_typedef_array_known_size
    , spec_bindingSpecs_fun_arg_typedef_enum
    , spec_bindingSpecs_fun_arg_typedef_function
    , spec_bindingSpecs_fun_arg_typedef_function_pointer
    , spec_bindingSpecs_fun_arg_typedef_struct
    , spec_bindingSpecs_fun_arg_typedef_union
      -- * Function arguments with macros
    , spec_bindingSpecs_fun_arg_macro_array
    , spec_bindingSpecs_fun_arg_macro_array_known_size
    , spec_bindingSpecs_fun_arg_macro_enum
    , spec_bindingSpecs_fun_arg_macro_function
    , spec_bindingSpecs_fun_arg_macro_function_pointer
    , spec_bindingSpecs_fun_arg_macro_struct
    , spec_bindingSpecs_fun_arg_macro_union
    ]

spec_bindingSpecs_bs_pre_omit_type :: TestCaseSpec
spec_bindingSpecs_bs_pre_omit_type =
    (defaultSpec "binding-specs/bs_pre_omit_type")
      { specPrescriptive = Just "examples/golden/binding-specs/bs_pre_omit_type_p.yaml"
      }

spec_bindingSpecs_bs_pre_name_squash_both :: TestCaseSpec
spec_bindingSpecs_bs_pre_name_squash_both =
    (defaultSpec "binding-specs/bs_pre_name_squash_both")
      { specPrescriptive = Just "examples/golden/binding-specs/bs_pre_name_squash_both_p.yaml"
      }

spec_bindingSpecs_bs_pre_name_squash_struct :: TestCaseSpec
spec_bindingSpecs_bs_pre_name_squash_struct =
    (defaultSpec "binding-specs/bs_pre_name_squash_struct")
      { specPrescriptive = Just "examples/golden/binding-specs/bs_pre_name_squash_struct_p.yaml"
      }

spec_bindingSpecs_bs_pre_name_squash_typedef :: TestCaseSpec
spec_bindingSpecs_bs_pre_name_squash_typedef =
    (defaultSpec "binding-specs/bs_pre_name_squash_typedef")
      { specPrescriptive = Just "examples/golden/binding-specs/bs_pre_name_squash_typedef_p.yaml"
      }

spec_bindingSpecs_bs_pre_name_type :: TestCaseSpec
spec_bindingSpecs_bs_pre_name_type =
    (defaultSpec "binding-specs/bs_pre_name_type")
      { specPrescriptive = Just "examples/golden/binding-specs/bs_pre_name_type_p.yaml"
      }

spec_bindingSpecs_macro_trans_dep_missing :: TestCaseSpec
spec_bindingSpecs_macro_trans_dep_missing =
    (defaultSpec "binding-specs/macro_trans_dep_missing")
      { specExternal = ["examples/golden/binding-specs/macro_trans_dep_missing.yaml"]
      , onFrontend = #selectPredicate .~ BIf (SelectDecl (DeclNameMatches "B|foo"))
      }

-- | Select predicate for fun_arg_typedef tests
spec_bindingSpecs_fun_arg_typedef_selectPredicate :: Boolean SelectPredicate
spec_bindingSpecs_fun_arg_typedef_selectPredicate =
    BOr (BIf $ SelectDecl (DeclNameMatches "A|B|C|D|E|(My.*)"))
        (BIf $ SelectDecl (DeclNameMatches "(foo.*)|(bar.*)"))

spec_bindingSpecs_fun_arg_typedef :: FilePath -> TestCaseSpec
spec_bindingSpecs_fun_arg_typedef path =
    (defaultSpec path)
      { specExternal = ["examples" </> "golden" </> path <.> "yaml"]
      , onFrontend = #selectPredicate .~ spec_bindingSpecs_fun_arg_typedef_selectPredicate
      }

spec_bindingSpecs_fun_arg_typedef_array :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_array =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/array"

spec_bindingSpecs_fun_arg_typedef_array_known_size :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_array_known_size =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/array_known_size"

spec_bindingSpecs_fun_arg_typedef_enum :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_enum =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/enum"

spec_bindingSpecs_fun_arg_typedef_function :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_function =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/function"

spec_bindingSpecs_fun_arg_typedef_function_pointer :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_function_pointer =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/function_pointer"

spec_bindingSpecs_fun_arg_typedef_struct :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_struct =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/struct"

spec_bindingSpecs_fun_arg_typedef_union :: TestCaseSpec
spec_bindingSpecs_fun_arg_typedef_union =
    spec_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/union"

-- | Select predicate for fun_arg_macro tests
spec_bindingSpecs_fun_arg_macro_selectPredicate :: Boolean SelectPredicate
spec_bindingSpecs_fun_arg_macro_selectPredicate =
    BOr (BIf $ SelectDecl (DeclNameMatches "A|B|C|D|E|(My.*)"))
        (BIf $ SelectDecl (DeclNameMatches "(foo.*)|(bar.*)"))

spec_bindingSpecs_fun_arg_macro :: FilePath -> TestCaseSpec
spec_bindingSpecs_fun_arg_macro path =
    (defaultSpec path)
      { specExternal = ["examples" </> "golden" </> path <.> "yaml"]
      , onFrontend = #selectPredicate .~ spec_bindingSpecs_fun_arg_macro_selectPredicate
      }

spec_bindingSpecs_fun_arg_macro_array :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_array =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/array"

spec_bindingSpecs_fun_arg_macro_array_known_size :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_array_known_size =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/array_known_size"

spec_bindingSpecs_fun_arg_macro_enum :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_enum =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/enum"

spec_bindingSpecs_fun_arg_macro_function :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_function =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/function"

spec_bindingSpecs_fun_arg_macro_function_pointer :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_function_pointer =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/function_pointer"

spec_bindingSpecs_fun_arg_macro_struct :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_struct =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/struct"

spec_bindingSpecs_fun_arg_macro_union :: TestCaseSpec
spec_bindingSpecs_fun_arg_macro_union =
    spec_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/union"

{-------------------------------------------------------------------------------
  Bespoke tests: declarations
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_declarations :: [TestCaseSpec]
testCaseSpecs_bespoke_declarations = [
      spec_declarations_declaration_unselected_b
    , spec_declarations_definitions
    , spec_declarations_failing_tentative_definitions_linkage
    , spec_declarations_name_collision
    , spec_declarations_redeclaration
    , spec_declarations_redeclaration_different
    , spec_declarations_select_scoping
    , spec_declarations_tentative_definitions
    ]

spec_declarations_declaration_unselected_b :: TestCaseSpec
spec_declarations_declaration_unselected_b =
    defaultSpec "declarations/declaration_unselected_b"

spec_declarations_definitions :: TestCaseSpec
spec_declarations_definitions = defaultSpec "declarations/definitions"

spec_declarations_failing_tentative_definitions_linkage :: TestCaseSpec
spec_declarations_failing_tentative_definitions_linkage =
    (defaultSpec "declarations/failing/tentative_definitions_linkage")
      { outcome = FailureLibclang
      }

spec_declarations_name_collision :: TestCaseSpec
spec_declarations_name_collision = defaultSpec "declarations/name_collision"

spec_declarations_redeclaration :: TestCaseSpec
spec_declarations_redeclaration = defaultSpec "declarations/redeclaration"

spec_declarations_redeclaration_different :: TestCaseSpec
spec_declarations_redeclaration_different =
    defaultSpec "declarations/redeclaration_different"

spec_declarations_select_scoping :: TestCaseSpec
spec_declarations_select_scoping =
    (defaultSpec "declarations/select_scoping")
      { onFrontend = \cfg -> cfg
          & #parsePredicate  .~ BIf (ParseHeader FromMainHeaders)
          & #selectPredicate .~ BTrue
      }

spec_declarations_tentative_definitions :: TestCaseSpec
spec_declarations_tentative_definitions =
    defaultSpec "declarations/tentative_definitions"

{-------------------------------------------------------------------------------
  Bespoke tests: documentation
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_documentation :: [TestCaseSpec]
testCaseSpecs_bespoke_documentation = [
      spec_documentation_doxygen_docs
    ]

spec_documentation_doxygen_docs :: TestCaseSpec
spec_documentation_doxygen_docs =
    (defaultSpec "documentation/doxygen_docs")
      { clangVersion = Just (>= (19, 0, 0))
      }

{-------------------------------------------------------------------------------
  Bespoke tests: edge cases
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_edgeCases :: [TestCaseSpec]
testCaseSpecs_bespoke_edgeCases = [
      spec_edgeCases_clang_generated_collision
    , spec_edgeCases_duplicate
    , spec_edgeCases_headers
    , spec_edgeCases_iterator
    , spec_edgeCases_ordinary_anon
    , spec_edgeCases_select_no_match
    , spec_edgeCases_thread_local
    , spec_edgeCases_unsupported_builtin
    ]

spec_edgeCases_clang_generated_collision :: TestCaseSpec
spec_edgeCases_clang_generated_collision =
    (defaultSpec "edge-cases/clang_generated_collision")
      { clangVersion = Just (>= (16, 0, 0))
      }

spec_edgeCases_duplicate :: TestCaseSpec
spec_edgeCases_duplicate =
    (defaultSpec "edge-cases/duplicate")
      { onFrontend = \cfg -> cfg
          & #parsePredicate  .~ BTrue
          & #selectPredicate .~ BOr
              (BIf $ SelectDecl (DeclNameMatches "function"))
              (BIf $ SelectDecl (DeclNameMatches "duplicate"))
      }

spec_edgeCases_headers :: TestCaseSpec
spec_edgeCases_headers = defaultSpec "edge-cases/headers"

spec_edgeCases_iterator :: TestCaseSpec
spec_edgeCases_iterator =
    (defaultSpec "edge-cases/iterator")
      { clangVersion = Just (>= (15, 0, 0))
      , onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { enableBlocks = True } }
      }

spec_edgeCases_ordinary_anon :: TestCaseSpec
spec_edgeCases_ordinary_anon =
    (defaultSpec "edge-cases/ordinary_anon_parent")
      { onFrontend = #selectPredicate .~ BTrue
      }

spec_edgeCases_select_no_match :: TestCaseSpec
spec_edgeCases_select_no_match =
    (defaultSpec "edge-cases/select_no_match")
      { onFrontend = #selectPredicate .~
            BIf (SelectDecl (DeclNameMatches "this_pattern_will_never_match"))
      }

spec_edgeCases_thread_local :: TestCaseSpec
spec_edgeCases_thread_local =
    (defaultSpec "edge-cases/thread_local")
      { clangVersion = Just (>= (16, 0, 0))
      }

spec_edgeCases_unsupported_builtin :: TestCaseSpec
spec_edgeCases_unsupported_builtin = defaultSpec "edge-cases/unsupported_builtin"

{-------------------------------------------------------------------------------
  Bespoke tests: functions
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_functions :: [TestCaseSpec]
testCaseSpecs_bespoke_functions = [
      spec_functions_decls_in_signature
    , spec_functions_fun_attributes
    , spec_functions_fun_attributes_conflict
    , spec_functions_simple_func_rename
    , spec_functions_varargs
    ]

spec_functions_decls_in_signature :: TestCaseSpec
spec_functions_decls_in_signature =
    defaultSpec "functions/decls_in_signature"

spec_functions_fun_attributes :: TestCaseSpec
spec_functions_fun_attributes =
    (defaultSpec "functions/fun_attributes")
      { clangVersion = Just (>= (15, 0, 0))
      }

spec_functions_fun_attributes_conflict :: TestCaseSpec
spec_functions_fun_attributes_conflict =
    defaultSpec "functions/fun_attributes_conflict"

-- NOTE: This is a variant test - the golden tests handle variants specially
spec_functions_simple_func_rename :: TestCaseSpec
spec_functions_simple_func_rename =
    (defaultSpec "functions/simple_func")
      { name = "functions/simple_func.1.rename"
      }

spec_functions_varargs :: TestCaseSpec
spec_functions_varargs = defaultSpec "functions/varargs"

{-------------------------------------------------------------------------------
  Bespoke tests: globals
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_globals :: [TestCaseSpec]
testCaseSpecs_bespoke_globals = [
      spec_globals_globals
    ]

spec_globals_globals :: TestCaseSpec
spec_globals_globals = defaultSpec "globals/globals"

{-------------------------------------------------------------------------------
  Bespoke tests: macros
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_macros :: [TestCaseSpec]
testCaseSpecs_bespoke_macros = [
      spec_macros_macro_in_fundecl
    , spec_macros_macro_in_fundecl_vs_typedef
    , spec_macros_macro_redefines_global
    , spec_macros_reparse
    ]

spec_macros_macro_in_fundecl :: TestCaseSpec
spec_macros_macro_in_fundecl = defaultSpec "macros/macro_in_fundecl"

spec_macros_macro_in_fundecl_vs_typedef :: TestCaseSpec
spec_macros_macro_in_fundecl_vs_typedef =
    defaultSpec "macros/macro_in_fundecl_vs_typedef"

spec_macros_macro_redefines_global :: TestCaseSpec
spec_macros_macro_redefines_global = defaultSpec "macros/macro_redefines_global"

spec_macros_reparse :: TestCaseSpec
spec_macros_reparse =
    (defaultSpec "macros/reparse")
      { clangVersion = Just (>= (15, 0, 0))
      , onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { argsBefore = ["-std=c2x"] } }
      }

{-------------------------------------------------------------------------------
  Bespoke tests: program analysis
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_programAnalysis :: [TestCaseSpec]
testCaseSpecs_bespoke_programAnalysis = [
      spec_programAnalysis_delay_traces
    , spec_programAnalysis_program_slicing_selection
    , spec_programAnalysis_program_slicing_simple
    , spec_programAnalysis_selection_bad
    , spec_programAnalysis_selection_fail
    , spec_programAnalysis_selection_fail_variant_1
    , spec_programAnalysis_selection_fail_variant_2
    , spec_programAnalysis_selection_fail_variant_3
    , spec_programAnalysis_selection_foo
    , spec_programAnalysis_selection_merge_traces
    , spec_programAnalysis_selection_omit_external_a
    , spec_programAnalysis_selection_omit_external_b
    , spec_programAnalysis_selection_omit_prescriptive
    , spec_programAnalysis_selection_squash
    ]

spec_programAnalysis_delay_traces :: TestCaseSpec
spec_programAnalysis_delay_traces =
    (defaultSpec "program-analysis/delay_traces")
      { onFrontend = #selectPredicate .~
            BOr
              (BIf $ SelectDecl (DeclNameMatches "_function"))
              (BIf $ SelectDecl (DeclNameMatches "struct"))
      }

spec_programAnalysis_program_slicing_selection :: TestCaseSpec
spec_programAnalysis_program_slicing_selection =
    (defaultSpec "program-analysis/program_slicing_selection")
      { onFrontend = \cfg -> cfg
          & #parsePredicate .~ BTrue
          & #selectPredicate .~ BOr
              (BIf . SelectDecl $ DeclNameMatches "FileOperationRecord")
              (BIf . SelectDecl $ DeclNameMatches "read_file_chunk")
          & #programSlicing .~ EnableProgramSlicing
      }

spec_programAnalysis_program_slicing_simple :: TestCaseSpec
spec_programAnalysis_program_slicing_simple =
    (defaultSpec "program-analysis/program_slicing_simple")
      { onFrontend = \cfg -> cfg
          & #parsePredicate  .~ BTrue
          & #selectPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing  .~ EnableProgramSlicing
      , specStdlib = BindingSpec.DisableStdlibBindingSpec
      , specExternal = ["examples/golden/program-analysis/program_slicing_simple.yaml"]
      }

spec_programAnalysis_selection_bad :: TestCaseSpec
spec_programAnalysis_selection_bad =
    defaultSpec "program-analysis/selection_bad"

spec_programAnalysis_selection_fail :: TestCaseSpec
spec_programAnalysis_selection_fail =
    defaultSpec "program-analysis/selection_fail"

spec_programAnalysis_selection_fail_variant_1 :: TestCaseSpec
spec_programAnalysis_selection_fail_variant_1 =
    (defaultSpec "program-analysis/selection_fail")
      { name = "program-analysis/selection_fail.1.deselect_failed"
      , onFrontend = #selectPredicate .~ BAnd
            (       BIf $ SelectHeader   FromMainHeaders)
            (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
      }

spec_programAnalysis_selection_fail_variant_2 :: TestCaseSpec
spec_programAnalysis_selection_fail_variant_2 =
    (defaultSpec "program-analysis/selection_fail")
      { name = "program-analysis/selection_fail.2.program_slicing"
      , onFrontend = \cfg -> cfg
          & #selectPredicate .~ BAnd
              (       BIf $ SelectHeader   FromMainHeaders)
              (BNot $ BIf $ SelectDecl   $ DeclNameMatches "struct Fail")
          & #programSlicing .~ EnableProgramSlicing
      }

spec_programAnalysis_selection_fail_variant_3 :: TestCaseSpec
spec_programAnalysis_selection_fail_variant_3 =
    (defaultSpec "program-analysis/selection_fail")
      { name = "program-analysis/selection_fail.3.select_ok"
      , onFrontend = \cfg -> cfg
          & #selectPredicate .~ BAnd
              (       BIf $ SelectDecl $ DeclNameMatches "struct OkBefore")
              (BNot $ BIf $ SelectDecl $ DeclNameMatches "struct Fail")
          & #programSlicing .~ EnableProgramSlicing
      }

spec_programAnalysis_selection_foo :: TestCaseSpec
spec_programAnalysis_selection_foo =
    defaultSpec "program-analysis/selection_foo"

spec_programAnalysis_selection_merge_traces :: TestCaseSpec
spec_programAnalysis_selection_merge_traces =
    (defaultSpec "program-analysis/selection_merge_traces")
      { onFrontend = #selectPredicate .~
            BAnd
              (BIf (SelectHeader FromMainHeaders)) (
            BAnd
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct X")))
              (BNot (BIf (SelectDecl $ DeclNameMatches "struct Y"))))
      }

spec_programAnalysis_selection_omit_external_a :: TestCaseSpec
spec_programAnalysis_selection_omit_external_a =
    (defaultSpec "program-analysis/selection_omit_external_a")
      { specExternal = ["examples/golden/program-analysis/selection_omit_external.yaml"]
      , onFrontend = #programSlicing .~ EnableProgramSlicing
      }

spec_programAnalysis_selection_omit_external_b :: TestCaseSpec
spec_programAnalysis_selection_omit_external_b =
    (defaultSpec "program-analysis/selection_omit_external_b")
      { specExternal = ["examples/golden/program-analysis/selection_omit_external.yaml"]
      , onFrontend = #programSlicing .~ EnableProgramSlicing
      }

spec_programAnalysis_selection_omit_prescriptive :: TestCaseSpec
spec_programAnalysis_selection_omit_prescriptive =
    (defaultSpec "program-analysis/selection_omit_prescriptive")
      { specPrescriptive = Just "examples/golden/program-analysis/selection_omit_prescriptive.yaml"
      }

spec_programAnalysis_selection_squash :: TestCaseSpec
spec_programAnalysis_selection_squash =
    defaultSpec "program-analysis/selection_squash_typedef"

{-------------------------------------------------------------------------------
  Bespoke tests: types
-------------------------------------------------------------------------------}

testCaseSpecs_bespoke_types :: [TestCaseSpec]
testCaseSpecs_bespoke_types = [
      spec_types_implicit_fields_struct
    , spec_types_implicit_fields_union
    , spec_types_long_double
    , spec_types_primitives_bool_c23
    , spec_types_special_parse_failure_long_double
    , spec_types_structs_named_vs_anon
    , spec_types_structs_unnamed_struct
    , spec_types_typedefs_typedefs
    , spec_types_typedefs_typenames
    ]

-- | The complex tests use 'complex' as a variable name, which conflicts with
-- the C99+ 'complex' keyword. On Windows, clang defaults to treating 'complex'
-- as a keyword, so we need to force C89 mode where it's not reserved.
--
spec_types_complex_non_float_test :: TestCaseSpec
spec_types_complex_non_float_test =
#ifdef mingw32_HOST_OS
    (defaultSpec "types/complex/complex_non_float_test")
      { onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { argsBefore = ["-std=gnu89"] } } }
#else
    defaultSpec "types/complex/complex_non_float_test"
#endif

spec_types_complex_hsb_complex_test :: TestCaseSpec
spec_types_complex_hsb_complex_test =
#ifdef mingw32_HOST_OS
    (defaultSpec "types/complex/hsb_complex_test")
      { onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { argsBefore = ["-std=gnu89"] } } }
#else
    defaultSpec "types/complex/hsb_complex_test"
#endif

spec_types_implicit_fields_struct :: TestCaseSpec
spec_types_implicit_fields_struct =
    defaultSpec "types/structs/implicit_fields_struct"

spec_types_implicit_fields_union :: TestCaseSpec
spec_types_implicit_fields_union =
    defaultSpec "types/unions/implicit_fields_union"

spec_types_long_double :: TestCaseSpec
spec_types_long_double = defaultSpec "types/special/long_double"

spec_types_primitives_bool_c23 :: TestCaseSpec
spec_types_primitives_bool_c23 =
    (defaultSpec "types/primitives/bool_c23")
      { clangVersion = Just (>= (15, 0, 0))
      , onBoot = \cfg -> cfg { clangArgs = cfg.clangArgs { argsBefore = ["-std=c2x"] } }
      }

spec_types_special_parse_failure_long_double :: TestCaseSpec
spec_types_special_parse_failure_long_double =
    defaultSpec "types/special/parse_failure_long_double"

spec_types_structs_named_vs_anon :: TestCaseSpec
spec_types_structs_named_vs_anon =
    (defaultSpec "types/structs/named_vs_anon")
      { clangVersion = Just (>= (19, 1, 0))
      }

spec_types_structs_unnamed_struct :: TestCaseSpec
spec_types_structs_unnamed_struct =
    defaultSpec "types/structs/unnamed-struct"

spec_types_typedefs_typedefs :: TestCaseSpec
spec_types_typedefs_typedefs = defaultSpec "types/typedefs/typedefs"

spec_types_typedefs_typenames :: TestCaseSpec
spec_types_typedefs_typenames = defaultSpec "types/typedefs/typenames"
