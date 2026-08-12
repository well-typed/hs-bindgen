-- | Golden tests: types
module Test.HsBindgen.Golden.Types (testCases) where

import System.FilePath ((<.>), (</>))

import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (UnusableReason (..))
import HsBindgen.Frontend.Pass.MangleNames.Error
import HsBindgen.Frontend.Pass.Parse.Msg
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.TraceMsg

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      -- Default tests
      defaultTest "types/anonymous/edge-cases/anon_in_nonanon"
    , defaultTest "types/anonymous/edge-cases/bitfield"
    , defaultTest "types/anonymous/edge-cases/duplicate_field_names"
    , defaultTest "types/anonymous/edge-cases/padding"
    , defaultTest "types/anonymous/struct_in_struct"
    , defaultTest "types/anonymous/struct_in_union"
    , defaultTest "types/anonymous/struct"
    , defaultTest "types/anonymous/union_in_struct"
    , defaultTest "types/anonymous/union_in_union"
    , defaultTest "types/anonymous/union"
    , defaultTest "types/complex/complex_non_float_test"
    , defaultTest "types/complex/hsb_complex_test"
    , defaultTest "types/complex/vector_test"
    , defaultTest "types/enums/enum_cpp_syntax"
    , defaultTest "types/enums/enum_unsigned_values"
    , defaultTest "types/enums/enums"
    , defaultTest "types/enums/nested_enums"
    , defaultTest "types/enums/untagged_enum_toplevel"
    , defaultTest "types/nested/nested_types"
    , defaultTest "types/primitives/bool"
    , defaultTest "types/primitives/fixedwidth"
    , defaultTest "types/primitives/primitive_insts"
    , defaultTest "types/primitives/primitive_types"
    , defaultTest "types/qualifiers/const_typedefs"
    , defaultTest "types/qualifiers/type_qualifiers"
    , defaultTest "types/structs/circular_dependency_struct"
    , defaultTest "types/structs/padding"
    , defaultTest "types/structs/recursive_struct"
    , defaultTest "types/structs/simple_structs"
    , defaultTest "types/structs/struct_arg"
    , defaultTest "types/structs/untagged"
    , defaultTest "types/typedefs/auxiliary/function-pointer/array"
    , defaultTest "types/typedefs/auxiliary/function-pointer/direct"
    , defaultTest "types/typedefs/auxiliary/function-pointer/multi_level"
    , defaultTest "types/typedefs/auxiliary/function-pointer/pointer"
    , defaultTest "types/typedefs/auxiliary/function-pointer/qual"
    , defaultTest "types/typedefs/typedef_vs_macro"
    , defaultTest "types/unions/bitfields"
    , defaultTest "types/unions/nested_unions"
    , defaultTest "types/unions/unions"
    ]
      -- Indirect fields
    ++ test_indirect_fields
    ++ [
      -- Bespoke tests
      test_anonymous_edge_cases_drop_indirect_fields_1
    , test_anonymous_edge_cases_drop_indirect_fields_2
    , test_anonymous_edge_cases_empty_anon
    , test_anonymous_edge_cases_multi_nesting
    , test_anonymous_edge_cases_multi_nesting_omit_field_prefixes
    , test_anonymous_edge_cases_reparse
    , test_long_double
    , test_primitives_bool_c23
    , test_primitives_bool_macro_override
    , test_primitives_bool_typedef_override
    , test_primitives_least_fast
    , test_scoping_deep_nesting
    , test_scoping_nesting
    , test_scoping_wide_nesting
    , test_special_parse_failure_long_double
    , test_structs_bitfields
    , test_structs_omit_field_prefixes
    , test_structs_post_qualified
    , test_structs_tagged_vs_untagged
    , test_structs_untagged_struct
    , test_typedefs_auxiliary_function_pointer_block
    , test_typedefs_typedefs
    , test_typedefs_typenames
    ]

{-------------------------------------------------------------------------------
  Indirect fields
-------------------------------------------------------------------------------}

-- | Tests related to indirect fields
--
-- Indirect fields are handled the same for structs as for unions, so it does
-- not matter whether we nest structs or unions or a combination of them. We
-- arbitrarily decided to use structs in these test headers.
test_indirect_fields :: [TestCase]
test_indirect_fields = [
      defaultTest "types/anonymous/indirect-fields/multi_nesting_enum_tagged"
    , defaultTest "types/anonymous/indirect-fields/multi_nesting_enum_untagged"
    , defaultTest "types/anonymous/indirect-fields/multi_nesting_struct_tagged"
    , defaultTest "types/anonymous/indirect-fields/multi_nesting_struct_untagged"
    , defaultTest "types/anonymous/indirect-fields/multi_nesting_union_tagged"
    , defaultTest "types/anonymous/indirect-fields/multi_nesting_union_untagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_enum_tagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_enum_untagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_struct_tagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_struct_untagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_union_tagged"
    , defaultTest "types/anonymous/indirect-fields/single_nesting_union_untagged"
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

-- | Test that indirect fields are dropped if they would cross an external
-- binding spec abstraction boundary
test_anonymous_edge_cases_drop_indirect_fields_1 :: TestCase
test_anonymous_edge_cases_drop_indirect_fields_1 =
    defaultTest "types/anonymous/edge-cases/drop_indirect_fields"
      & #name %~ (++ "_1")
      & #outputDir %~ (++ "_1")
      & #specExternal .~
          [ "test-artefacts/headers/golden"
            </> "types/anonymous/edge-cases/drop_indirect_fields_1"
            <.> "ext" <.> "yaml"
          ]
      & #tracePredicate .~ multiTracePredicate expected trace
  where
    expected :: [(C.DeclName, C.ScopedName)]
    expected = [
        ("struct S", "x")
      , ("struct S", "y")
      , ("struct S_anon'anon'x", "x")
      , ("struct S_anon'anon'x", "y")
      ]

    trace :: TraceMsg -> Maybe (TraceExpectation (C.DeclName, C.ScopedName))
    trace = \case
      MatchResolveBindingSpecs (ResolveBindingSpecsIndirectFieldDropped cDeclId cFieldName) ->
        Just $ Expected (cDeclId.name, cFieldName)
      _otherwise ->
        Nothing

-- | Test that indirect fields are dropped if they would cross an external
-- binding spec abstraction boundary
test_anonymous_edge_cases_drop_indirect_fields_2 :: TestCase
test_anonymous_edge_cases_drop_indirect_fields_2 =
    defaultTest "types/anonymous/edge-cases/drop_indirect_fields"
      & #name %~ (++ "_2")
      & #outputDir %~ (++ "_2")
      & #specExternal .~
          [ "test-artefacts/headers/golden"
            </> "types/anonymous/edge-cases/drop_indirect_fields_2"
            <.> "ext" <.> "yaml"
          ]
      & #tracePredicate .~ multiTracePredicate expected trace
  where
    expected :: [(C.DeclName, C.ScopedName)]
    expected = [
        ("struct S", "x")
      , ("struct S", "y")
      , ("struct S", "z")
      ]

    trace :: TraceMsg -> Maybe (TraceExpectation (C.DeclName, C.ScopedName))
    trace = \case
      MatchResolveBindingSpecs (ResolveBindingSpecsIndirectFieldDropped cDeclId cFieldName) ->
        Just $ Expected (cDeclId.name, cFieldName)
      _otherwise ->
        Nothing

test_anonymous_edge_cases_empty_anon :: TestCase
test_anonymous_edge_cases_empty_anon =
    testTraceMulti "types/anonymous/edge-cases/empty_anon" declsWithMsgs $ \case
      MatchDelayedImplicitField "struct S1" UnsupportedEmptyAnon ->
        Just $ Expected "struct S1"
      MatchDelayed "struct S2" ParseNestedDeclsFailed ->
        Just $ Expected "struct S2"
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct S1", "struct S2"]

test_anonymous_edge_cases_multi_nesting :: TestCase
test_anonymous_edge_cases_multi_nesting =
    defaultTest "types/anonymous/edge-cases/multi_nesting"

test_anonymous_edge_cases_multi_nesting_omit_field_prefixes :: TestCase
test_anonymous_edge_cases_multi_nesting_omit_field_prefixes =
    testVariant "types/anonymous/edge-cases/multi_nesting" Nothing "omit_field_prefixes"
      & #onFrontend .~ ( #fieldNamingStrategy .~ OmitFieldPrefixes )

-- | Test that indirect fields are reparsed
test_anonymous_edge_cases_reparse :: TestCase
test_anonymous_edge_cases_reparse =
    defaultTest "types/anonymous/edge-cases/reparse"

test_long_double :: TestCase
test_long_double =
    testTraceSimple "types/special/long_double" $ \case
      MatchDelayed _name ParseUnsupportedLongDouble ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_primitives_bool_c23 :: TestCase
test_primitives_bool_c23 =
    defaultTest "types/primitives/bool_c23"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23

-- | Test that when @bool@ is given a definition (via @#define@), it is /not/
-- parsed as @PrimBool@ by @language-c@.
test_primitives_bool_macro_override :: TestCase
test_primitives_bool_macro_override =
    defaultTest "types/primitives/bool_macro_override"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23

-- | Test that when @bool@ is given a definition (via @typedef@), it is /not/
-- parsed as @PrimBool@ by @language-c@.
test_primitives_bool_typedef_override :: TestCase
test_primitives_bool_typedef_override =
    defaultTest "types/primitives/bool_typedef_override"

test_primitives_least_fast :: TestCase
test_primitives_least_fast =
    defaultTest "types/primitives/least_fast"
      & #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )

test_scoping_deep_nesting :: TestCase
test_scoping_deep_nesting =
    testTraceMulti "types/scoping/deep_nesting" declsWithMsgs $ \case
      MatchDelayed name@"struct foo" ParseNestedDeclsFailed ->
        Just $ Expected name
      MatchDelayed name@"struct bar" ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct foo", "struct bar"]

test_scoping_nesting :: TestCase
test_scoping_nesting =
    testTraceMulti "types/scoping/nesting" declsWithMsgs $ \case
      MatchDelayed name@"struct foo" ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct foo"]

test_scoping_wide_nesting :: TestCase
test_scoping_wide_nesting =
    testTraceMulti "types/scoping/wide_nesting" declsWithMsgs $ \case
      MatchDelayed name@"struct foo" ParseNestedDeclsFailed ->
        Just $ Expected name
      MatchDelayed name@"struct bar" ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct foo", "struct bar"]

test_special_parse_failure_long_double :: TestCase
test_special_parse_failure_long_double =
    testTraceMulti "types/special/parse_failure_long_double" declsWithMsgs $ \case
      MatchDelayed name ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["fun1", "struct struct1"]

test_structs_tagged_vs_untagged :: TestCase
test_structs_tagged_vs_untagged =
    defaultTest "types/structs/tagged_vs_untagged"
      & #clangVersion .~ Just (>= (19, 1, 0))

test_structs_bitfields :: TestCase
test_structs_bitfields =
    defaultTest "types/structs/bitfields"
      & #cStandard .~ c99  -- C99 required for inline functions

test_structs_omit_field_prefixes :: TestCase
test_structs_omit_field_prefixes =
    testVariant "types/structs/simple_structs" Nothing "omit_field_prefixes"
      & #onFrontend .~ ( #fieldNamingStrategy .~ OmitFieldPrefixes )

test_structs_post_qualified :: TestCase
test_structs_post_qualified =
    testVariant "types/structs/simple_structs" Nothing "post_qualified"
      & #qualifiedStyle .~ PostQualified

test_structs_untagged_struct :: TestCase
test_structs_untagged_struct =
    testTraceSimple "types/structs/untagged_struct" $ \case
      MatchDiagnosticCategory "Semantic Issue" ->
        Just $ Expected ()
      MatchNoDeclarations ->
        Just $ Tolerated
      _otherwise ->
        Nothing

test_typedefs_auxiliary_function_pointer_block :: TestCase
test_typedefs_auxiliary_function_pointer_block =
    defaultTest "types/typedefs/auxiliary/function-pointer/block"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23
      & #onBoot       .~ ( #clangArgs % #enableBlocks .~ True )

test_typedefs_typedefs :: TestCase
test_typedefs_typedefs =
    testTraceMulti "types/typedefs/typedefs" declsWithMsgs $ \case
      MatchDelayed name ParseFunctionOfTypeTypedef ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["foo"]

-- This tests https://github.com/well-typed/hs-bindgen/issues/1389.
test_typedefs_typenames :: TestCase
test_typedefs_typenames =
    testTraceMulti "types/typedefs/typenames" declsWithMsgs $ \case
      MatchUnusable name (UnusableMangleNamesFailure (MangleNamesCollisionError DetectClashesCollision{})) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["enum foo", "foo"]
