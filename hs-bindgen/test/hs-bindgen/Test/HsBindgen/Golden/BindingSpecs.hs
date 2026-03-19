-- | Golden tests: binding specs
module Test.HsBindgen.Golden.BindingSpecs (testCases) where

import System.FilePath ((<.>), (</>))

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
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
      test_bindingSpecs_omit_type
      -- * Bugs / regression tests
    , test_bindingSpecs_macro_trans_dep_missing
      -- * Naming types
    , test_bindingSpecs_name_squash_both
    , test_bindingSpecs_name_squash_struct
    , test_bindingSpecs_name_squash_typedef
    , test_bindingSpecs_name_type
      -- * Representation: emptydata
    , test_bindingSpecs_rep_emptydata_enum
    , test_bindingSpecs_rep_emptydata_macro_type
    , test_bindingSpecs_rep_emptydata_opaque
    , test_bindingSpecs_rep_emptydata_struct
    , test_bindingSpecs_rep_emptydata_typedef
    , test_bindingSpecs_rep_emptydata_union
      -- * Function arguments with typedefs
    , test_bindingSpecs_fun_arg_typedef_array
    , test_bindingSpecs_fun_arg_typedef_array_known_size
    , test_bindingSpecs_fun_arg_typedef_enum
    , test_bindingSpecs_fun_arg_typedef_function
    , test_bindingSpecs_fun_arg_typedef_function_pointer
    , test_bindingSpecs_fun_arg_typedef_struct
    , test_bindingSpecs_fun_arg_typedef_union
      -- * Function arguments with macros
    , test_bindingSpecs_fun_arg_macro_array
    , test_bindingSpecs_fun_arg_macro_array_known_size
    , test_bindingSpecs_fun_arg_macro_enum
    , test_bindingSpecs_fun_arg_macro_function
    , test_bindingSpecs_fun_arg_macro_function_pointer
    , test_bindingSpecs_fun_arg_macro_struct
    , test_bindingSpecs_fun_arg_macro_union
      -- * Standard library
    , test_bindingSpecs_stdlib_instances_c11
    , test_bindingSpecs_stdlib_instances_c23
    , test_bindingSpecs_stdlib_bool_c23
    , test_bindingSpecs_stdlib_return_values
    ]

{-------------------------------------------------------------------------------
  Omit type
-------------------------------------------------------------------------------}

test_bindingSpecs_omit_type :: TestCase
test_bindingSpecs_omit_type =
    defaultTest "binding-specs/omit_type"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/omit_type_p.yaml"

{-------------------------------------------------------------------------------
  Bugs / regression tests
-------------------------------------------------------------------------------}

-- | External binding specifications for macro types cause incorrect
-- TransitiveDependenciesMissing warnings
--
-- TODO <https://github.com/well-typed/hs-bindgen/issues/1513>
-- We currently report the wrong declaration as missing.
test_bindingSpecs_macro_trans_dep_missing :: TestCase
test_bindingSpecs_macro_trans_dep_missing =
    defaultTest "binding-specs/macro_trans_dep_missing"
      & #specExternal .~
          [ "examples/golden/binding-specs/macro_trans_dep_missing.yaml"
          ]
      & #onFrontend .~
          #selectPredicate .~ BIf (SelectDecl (DeclNameMatches "B|foo"))
      & #tracePredicate .~ multiTracePredicate ["foo" :: CDeclName] (\case
            -- no macros should fail to parse
            MatchHandleMacros _ ->
              Just Unexpected
            -- TODO <https://github.com/well-typed/hs-bindgen/issues/1513>
            -- Once the warning is fixed, this case can be removed.
            MatchSelect name (MatchTransMissing [MatchTransNotSelected]) ->
              Just (Expected name)
            _otherwise ->
              Nothing
          )

{-------------------------------------------------------------------------------
  Naming types
-------------------------------------------------------------------------------}

-- | Naming a squashed type, specifying the name for both the struct and typedef
test_bindingSpecs_name_squash_both :: TestCase
test_bindingSpecs_name_squash_both =
    defaultTest "binding-specs/name/squash_both"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/name/squash_both_p.yaml"

-- | Naming a squashed type, specifying the name for the struct
test_bindingSpecs_name_squash_struct :: TestCase
test_bindingSpecs_name_squash_struct =
    defaultTest "binding-specs/name/squash_struct"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/name/squash_struct_p.yaml"

-- | Naming a squashed type, specifying the name for the typedef
test_bindingSpecs_name_squash_typedef :: TestCase
test_bindingSpecs_name_squash_typedef =
    defaultTest "binding-specs/name/squash_typedef"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/name/squash_typedef_p.yaml"

-- | Naming a type
test_bindingSpecs_name_type :: TestCase
test_bindingSpecs_name_type =
    defaultTest "binding-specs/name/type"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/name/type_p.yaml"

{-------------------------------------------------------------------------------
  Representation: emptydata
-------------------------------------------------------------------------------}

-- | Making an enum type opaque
test_bindingSpecs_rep_emptydata_enum :: TestCase
test_bindingSpecs_rep_emptydata_enum =
    defaultTest "binding-specs/rep/emptydata/enum"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/enum_p.yaml"

-- | Making a macro type opaque
test_bindingSpecs_rep_emptydata_macro_type :: TestCase
test_bindingSpecs_rep_emptydata_macro_type =
    defaultTest "binding-specs/rep/emptydata/macro_type"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/macro_type_p.yaml"

-- | Opaque types work with representation emptydata
test_bindingSpecs_rep_emptydata_opaque :: TestCase
test_bindingSpecs_rep_emptydata_opaque =
    defaultTest "binding-specs/rep/emptydata/opaque"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/opaque_p.yaml"

-- | Making a struct type opaque
--
-- This test also checks that dependencies in other headers are not selected if
-- they are only used by a type that is made opaque.
--
-- This test also has tests for nested types.
test_bindingSpecs_rep_emptydata_struct :: TestCase
test_bindingSpecs_rep_emptydata_struct =
    defaultTest "binding-specs/rep/emptydata/struct"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/struct_p.yaml"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing .~ EnableProgramSlicing
          )

-- | Making a typedef type opaque
--
-- This test also checks that dependencies in other headers are not selected if
-- they are only used by a type that is made opaque.
test_bindingSpecs_rep_emptydata_typedef :: TestCase
test_bindingSpecs_rep_emptydata_typedef =
    defaultTest "binding-specs/rep/emptydata/typedef"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/typedef_p.yaml"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing .~ EnableProgramSlicing
          )

-- | Making a union type opaque
--
-- This test also checks that dependencies in other headers are not selected if
-- they are only used by a type that is made opaque.
--
-- This test also tests nested types.
test_bindingSpecs_rep_emptydata_union :: TestCase
test_bindingSpecs_rep_emptydata_union =
    defaultTest "binding-specs/rep/emptydata/union"
      & #specPrescriptive .~
          Just "examples/golden/binding-specs/rep/emptydata/union_p.yaml"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BIf (SelectHeader FromMainHeaders)
          & #programSlicing .~ EnableProgramSlicing
          )

{-------------------------------------------------------------------------------
  Function arguments with typedefs
-------------------------------------------------------------------------------}

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying array type (of
-- unknown size).
--
-- Arrays are passed by 'Ptr' to the first element of the array.
test_bindingSpecs_fun_arg_typedef_array :: TestCase
test_bindingSpecs_fun_arg_typedef_array =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/array"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying array type of
-- known size.
--
-- Arrays of known size are passed by 'Ptr' to the first element of the array.
test_bindingSpecs_fun_arg_typedef_array_known_size :: TestCase
test_bindingSpecs_fun_arg_typedef_array_known_size =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/array_known_size"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying enum type.
--
-- Enums can be passed by value rather than by 'Ptr'.
test_bindingSpecs_fun_arg_typedef_enum :: TestCase
test_bindingSpecs_fun_arg_typedef_enum =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/enum"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying function type.
--
-- Functions should be passed by 'FunPtr' rather than by 'Ptr'. Previously we
-- had a bug where we doing the latter, see issue #1363.
test_bindingSpecs_fun_arg_typedef_function :: TestCase
test_bindingSpecs_fun_arg_typedef_function =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/function"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying function pointer
-- type.
--
-- Functions should be passed by 'FunPtr' rather than by 'Ptr'.
test_bindingSpecs_fun_arg_typedef_function_pointer :: TestCase
test_bindingSpecs_fun_arg_typedef_function_pointer =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/function_pointer"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying struct type.
--
-- Structs should be passed by 'Ptr' rather than by value.
test_bindingSpecs_fun_arg_typedef_struct :: TestCase
test_bindingSpecs_fun_arg_typedef_struct =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/struct"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef with an underlying union type.
--
-- Union should be passed by 'Ptr' rather than by value.
test_bindingSpecs_fun_arg_typedef_union :: TestCase
test_bindingSpecs_fun_arg_typedef_union =
    test_bindingSpecs_fun_arg_typedef "binding-specs/fun_arg/typedef/union"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a typedef.
test_bindingSpecs_fun_arg_typedef :: FilePath -> TestCase
test_bindingSpecs_fun_arg_typedef path =
  defaultTest path
      & #specExternal .~
          [ "examples" </> "golden" </> path <.> "yaml"
          ]
      & #onFrontend .~
          #selectPredicate .~ test_bindingSpecs_fun_arg_typedef_selectPredicate

-- | Select predicate for 'test_bindingSpecs_fun_arg_typedef' tests
test_bindingSpecs_fun_arg_typedef_selectPredicate :: Boolean SelectPredicate
test_bindingSpecs_fun_arg_typedef_selectPredicate =
    BOr (BIf $ SelectDecl (DeclNameMatches "A|B|C|D|E|(My.*)"))
        (BIf $ SelectDecl (DeclNameMatches "(foo.*)|(bar.*)"))

{-------------------------------------------------------------------------------
  Function arguments with macros
-------------------------------------------------------------------------------}

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying array type (of
-- unknown size).
--
-- Arrays are passed by 'Ptr' to the first element of the array.
test_bindingSpecs_fun_arg_macro_array :: TestCase
test_bindingSpecs_fun_arg_macro_array =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/array"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying array type of
-- known size.
--
-- Arrays of known size are passed by 'Ptr' to the first element of the array.
test_bindingSpecs_fun_arg_macro_array_known_size :: TestCase
test_bindingSpecs_fun_arg_macro_array_known_size =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/array_known_size"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying enum type.
--
-- Enums can be passed by value rather than by 'Ptr'.
test_bindingSpecs_fun_arg_macro_enum :: TestCase
test_bindingSpecs_fun_arg_macro_enum =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/enum"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying function type.
--
-- Functions should be passed by 'FunPtr' rather than by 'Ptr'. Previously we
-- had a bug where we doing the latter, see issue #1363.
test_bindingSpecs_fun_arg_macro_function :: TestCase
test_bindingSpecs_fun_arg_macro_function =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/function"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying function
-- pointer type.
--
-- Functions should be passed by 'FunPtr' rather than by 'Ptr'.
test_bindingSpecs_fun_arg_macro_function_pointer :: TestCase
test_bindingSpecs_fun_arg_macro_function_pointer =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/function_pointer"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying struct type.
--
-- Structs should be passed by 'Ptr' rather than by value.
test_bindingSpecs_fun_arg_macro_struct :: TestCase
test_bindingSpecs_fun_arg_macro_struct =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/struct"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type with an underlying union type.
--
-- Union should be passed by 'Ptr' rather than by value.
test_bindingSpecs_fun_arg_macro_union :: TestCase
test_bindingSpecs_fun_arg_macro_union =
    test_bindingSpecs_fun_arg_macro "binding-specs/fun_arg/macro/union"

-- | Test that @hs-bindgen@ can detect whether an external binding reference in
-- a function argument references a macro type.
test_bindingSpecs_fun_arg_macro :: FilePath -> TestCase
test_bindingSpecs_fun_arg_macro path =
  defaultTest path
      & #specExternal .~
          [ "examples" </> "golden" </> path <.> "yaml"
          ]
      & #onFrontend .~
          #selectPredicate .~ test_bindingSpecs_fun_arg_macro_selectPredicate
      & #tracePredicate .~ test_bindingSpecs_fun_arg_macro_tracePredicate

-- | Select predicate for 'test_bindingSpecs_fun_arg_macro' tests
test_bindingSpecs_fun_arg_macro_selectPredicate :: Boolean SelectPredicate
test_bindingSpecs_fun_arg_macro_selectPredicate =
    BOr (BIf $ SelectDecl (DeclNameMatches "A|B|C|D|E|(My.*)"))
        (BIf $ SelectDecl (DeclNameMatches "(foo.*)|(bar.*)"))

-- | Trace predicate for 'test_bindingSpecs_fun_arg_macro' tests
test_bindingSpecs_fun_arg_macro_tracePredicate :: TracePredicate TraceMsg
test_bindingSpecs_fun_arg_macro_tracePredicate =
    noHandleMacrosTraces

noHandleMacrosTraces :: TracePredicate TraceMsg
noHandleMacrosTraces = multiTracePredicate ([] :: [String]) (\case
    -- no macros should fail to parse
    MatchHandleMacros _ ->
      Just Unexpected
    _otherwise ->
      Nothing
  )

{-------------------------------------------------------------------------------
  Standard library
-------------------------------------------------------------------------------}

-- The following test variants examine "expectations" about how we handle
-- @bool@.
--
-- | The translation of @bool@ involves unique pitfalls. Historically defined as
-- a macro in @stdbool.h@, it became a primitive keyword in C23.
--
-- We translate @bool@ to 'HsBindgen.Runtime.LibC.CBool'.
--
-- Given the C header file
--
-- @
-- #include <stdbool.h>
-- typedef bool Foo;
-- @
--
-- and @stdbool.h@
--
-- @
-- #define bool _Bool
-- @
--
-- Since @bool@ is a macro, @libclang@ will inform us about a macro expansion in
-- the definition of @Foo@. That is, we always reparse the @bool@ macro in
-- @Foo@.
--
-- The reparse environment depends on the C standard (in C23 we translate @bool@
-- directly to the primitive boolean type).
--
-- **Case 1: C11**
--
-- The definition of @bool@ is added to the reparse environment; reparsing
-- correctly recognizes @bool@ as an external reference, which gets matched
-- against the external binding spec. We translate to
-- 'HsBindgen.Runtime.LibC.CBool'.
test_bindingSpecs_stdlib_instances_c11 :: TestCase
test_bindingSpecs_stdlib_instances_c11 =
    testVariant "binding-specs/stdlib/instances" "1.c11-parse-all"
      & #cStandard  .~ c11

-- **Case 2: C23**
--
-- The definition of @bool@ replaces the original keyword entry of the reparse
-- environment; reparsing correctly recognizes @bool@ as an external reference,
-- which gets matched against the external binding spec. We translate to
-- 'HsBindgen.Runtime.LibC.CBool'.
test_bindingSpecs_stdlib_instances_c23 :: TestCase
test_bindingSpecs_stdlib_instances_c23 =
    testVariant "binding-specs/stdlib/instances" "1.c23-parse-all"
      & #clangVersion .~ Just (>= (18, 0, 0))
      & #cStandard    .~ c23

-- **Case 3: C23 without including @stdbool.h@**
test_bindingSpecs_stdlib_bool_c23 :: TestCase
test_bindingSpecs_stdlib_bool_c23 =
    defaultTest "binding-specs/stdlib/bool"
      & #clangVersion .~ Just (>= (18, 0, 0))
      & #cStandard    .~ c23

test_bindingSpecs_stdlib_return_values :: TestCase
test_bindingSpecs_stdlib_return_values =
    defaultTest "binding-specs/stdlib/return_values"
