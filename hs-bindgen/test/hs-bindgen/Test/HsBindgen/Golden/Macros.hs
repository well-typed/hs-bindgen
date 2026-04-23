-- | Golden tests: macros
module Test.HsBindgen.Golden.Macros (testCases) where

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
      -- Default tests
      defaultTest "macros/issue_890"
    , defaultTest "macros/macro_functions"
    , defaultTest "macros/macro_strings"
    , defaultTest "macros/macro_type_ptr_qualifiers"
    , defaultTest "macros/macro_type_void"
    , defaultTest "macros/macro_typedef_struct"
    , defaultTest "macros/macro_types"
    , defaultTest "macros/object_like_as_function_like"
    , defaultTest "macros/parse/first_parse_then_typecheck"
    , defaultTest "macros/parse/macro_typedef_scope"
      -- Bespoke tests
    , test_macros_macro_in_fundecl
    , test_macros_macro_in_fundecl_vs_typedef
    , test_macros_macro_redefines_global
    , test_macros_macros
    , test_macros_parse_simple
    , test_macros_parse_elaborate
    , test_macros_parse_intermittent_include
    , test_macros_parse_intermittent_include_conditional
    , test_macros_parse_macro_typedef_scope_multiple
    , test_macros_reparse
    , test_macros_reparse_arithmetic_types
    , test_macros_reparse_functions
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
    declsWithMsgs = [
        "stdin"
      , "macro stdin"
      , "stdout"
      , "macro stdout"
      , "stderr"
      , "macro stderr"
      ]

test_macros_macros :: TestCase
test_macros_macros =
    defaultTest "macros/macros"
      & #cStandard .~ c23

test_macros_parse_simple :: TestCase
test_macros_parse_simple =
    defaultTest "macros/parse/simple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BTrue
          )

test_macros_parse_elaborate :: TestCase
test_macros_parse_elaborate =
    defaultTest "macros/parse/elaborate"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BTrue
          )

test_macros_parse_intermittent_include :: TestCase
test_macros_parse_intermittent_include =
    defaultTest "macros/parse/intermittent_include"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BTrue
          )

test_macros_parse_intermittent_include_conditional :: TestCase
test_macros_parse_intermittent_include_conditional =
    defaultTest "macros/parse/intermittent_include_conditional"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BTrue
          )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchDiagnosticCategory "Lexical or Preprocessor Issue" ->
              Just $ Expected ("FOO", "redefinition")
            MatchSelect name SelectConflict{} ->
              Just $ Expected (name, "conflict")
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [(CDeclName, String)]
    declsWithMsgs = [
        ("FOO", "redefinition")
      , ("macro FOO", "conflict")
      ]

test_macros_parse_macro_typedef_scope_multiple :: TestCase
test_macros_parse_macro_typedef_scope_multiple =
    defaultTest "macros/parse/macro_typedef_scope_multiple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectPredicate .~ BTrue
          )

test_macros_reparse :: TestCase
test_macros_reparse =
    defaultTest "macros/reparse"
        -- `bool` is a keyword.
      & #clangVersion   .~ Just (>= (15, 0, 0))
        -- `bool` is a keyword.
      & #cStandard      .~ c23

test_macros_reparse_arithmetic_types :: TestCase
test_macros_reparse_arithmetic_types =
    defaultTest "macros/reparse_arithmetic_types"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name@"f29" (SelectParseFailure ParseUnsupportedLongDouble) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs = ["f29"]

test_macros_reparse_functions :: TestCase
test_macros_reparse_functions =
    defaultTest "macros/reparse_functions"
      -- C99 required for inline functions
      & #cStandard .~ c99
