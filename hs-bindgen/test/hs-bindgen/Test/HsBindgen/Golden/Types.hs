-- | Golden tests: types
module Test.HsBindgen.Golden.Types (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.MangleNames.Error (MangleNamesFailure (MangleNamesCollision))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
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
      defaultTest "types/complex/complex_non_float_test"
    , defaultTest "types/complex/hsb_complex_test"
    , defaultTest "types/complex/vector_test"
    , defaultTest "types/enums/anon_enum_toplevel"
    , defaultTest "types/enums/enum_cpp_syntax"
    , defaultTest "types/enums/enum_unsigned_values"
    , defaultTest "types/enums/enums"
    , defaultTest "types/enums/nested_enums"
    , defaultTest "types/nested/nested_types"
    , defaultTest "types/primitives/bool"
    , defaultTest "types/primitives/fixedwidth"
    , defaultTest "types/primitives/primitive_insts"
    , defaultTest "types/primitives/primitive_types"
    , defaultTest "types/qualifiers/const_typedefs"
    , defaultTest "types/qualifiers/type_qualifiers"
    , defaultTest "types/structs/anonymous"
    , defaultTest "types/structs/circular_dependency_struct"
    , defaultTest "types/structs/recursive_struct"
    , defaultTest "types/structs/simple_structs"
    , defaultTest "types/structs/struct_arg"
    , defaultTest "types/structs/unnamed_bitfield"
    , defaultTest "types/typedefs/multi_level_function_pointer"
    , defaultTest "types/typedefs/typedef_vs_macro"
    , defaultTest "types/unions/nested_unions"
    , defaultTest "types/unions/unions"
      -- Bespoke tests
    , test_types_implicit_fields_struct
    , test_types_implicit_fields_union
    , test_types_long_double
    , test_types_primitives_bool_c23
    , test_types_primitives_least_fast
    , test_types_special_parse_failure_long_double
    , test_types_structs_named_vs_anon
    , test_types_structs_bitfields
    , test_types_structs_omit_field_prefixes
    , test_types_structs_post_qualified
    , test_types_structs_unnamed_struct
    , test_types_typedefs_typedefs
    , test_types_typedefs_typenames
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

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
      MatchDelayed _name ParseUnsupportedLongDouble ->
        Just $ Expected ()
      _otherwise ->
        Nothing

test_types_primitives_bool_c23 :: TestCase
test_types_primitives_bool_c23 =
    defaultTest "types/primitives/bool_c23"
      & #clangVersion .~ Just (>= (15, 0, 0))
      & #cStandard    .~ c23

test_types_primitives_least_fast :: TestCase
test_types_primitives_least_fast =
    defaultTest "types/primitives/least_fast"
      & #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )

test_types_special_parse_failure_long_double :: TestCase
test_types_special_parse_failure_long_double =
    testTraceMulti "types/special/parse_failure_long_double" declsWithMsgs $ \case
      MatchDelayed name ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["fun1", "struct struct1"]

test_types_structs_named_vs_anon :: TestCase
test_types_structs_named_vs_anon =
    defaultTest "types/structs/named_vs_anon"
      & #clangVersion .~ Just (>= (19, 1, 0))

test_types_structs_bitfields :: TestCase
test_types_structs_bitfields =
    defaultTest "types/structs/bitfields"
      & #cStandard .~ c99  -- C99 required for inline functions

test_types_structs_omit_field_prefixes :: TestCase
test_types_structs_omit_field_prefixes =
    testVariant "types/structs/simple_structs" "omit_field_prefixes"
      & #onFrontend .~ ( #fieldNamingStrategy .~ OmitFieldPrefixes )

test_types_structs_post_qualified :: TestCase
test_types_structs_post_qualified =
    testVariant "types/structs/simple_structs" "post_qualified"
      & #qualifiedStyle .~ PostQualified

test_types_structs_unnamed_struct :: TestCase
test_types_structs_unnamed_struct =
    testTraceSimple "types/structs/unnamed_struct" $ \case
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
    declsWithMsgs :: [CDeclName]
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
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["enum foo", "foo"]
