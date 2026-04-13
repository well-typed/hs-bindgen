-- | Golden tests: macros
module Test.HsBindgen.Golden.Macros (testCases) where

import HsBindgen.Frontend.Naming
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
      defaultTest "macros/issue_890"
    , defaultTest "macros/macro_functions"
    , defaultTest "macros/macro_strings"
    , defaultTest "macros/macro_type_void"
    , defaultTest "macros/macro_typedef_scope"
    , defaultTest "macros/macro_typedef_struct"
    , defaultTest "macros/macro_types"
    , defaultTest "macros/object_like_as_function_like"
      -- Bespoke tests
    , test_macros_macro_in_fundecl
    , test_macros_macro_in_fundecl_vs_typedef
    , test_macros_macro_redefines_global
    , test_macros_macros
    , test_macros_reparse_cref_attributes
    , test_macros_reparse_gnu_attributes
    , test_macros_reparse_reparse
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_macros_macro_in_fundecl :: TestCase
test_macros_macro_in_fundecl =
    testTraceMulti "macros/macro_in_fundecl" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
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
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["quux1", "quux2", "wam1", "wam2"]

test_macros_macro_redefines_global :: TestCase
test_macros_macro_redefines_global =
    testTraceMulti "macros/macro_redefines_global" declsWithMsgs $ \case
      MatchSelect name SelectConflict{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [CDeclName]
    declsWithMsgs = ["stdin", "stdout", "stderr"]

test_macros_macros :: TestCase
test_macros_macros =
    defaultTest "macros/macros"
      & #cStandard .~ c23

test_macros_reparse_gnu_attributes :: TestCase
test_macros_reparse_gnu_attributes =
    defaultTest "macros/reparse/gnu_attributes"
      & #cStandard      .~ c23

test_macros_reparse_cref_attributes :: TestCase
test_macros_reparse_cref_attributes =
    defaultTest "macros/reparse/cref_attributes"
      & #cStandard      .~ c23

test_macros_reparse_reparse :: TestCase
test_macros_reparse_reparse =
    defaultTest "macros/reparse/reparse"
      & #clangVersion   .~ Just (>= (15, 0, 0)) -- parse 'bool'
      & #cStandard      .~ c23
      & #tracePredicate .~ tolerateAll
