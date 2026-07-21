-- | Golden tests: macros
module Test.HsBindgen.Golden.Macros (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Analysis.DeclIndex (UnusableReason (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

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
    , defaultTest "macros/undef"
      -- Bespoke tests
    , test_macros_macro_ext_binding_dep
    , test_macros_macro_resolution_log_level_default
    , test_macros_macro_resolution_log_level_warnings
    , test_macros_macro_type_unresolved_tagged
    , test_macros_macro_in_fundecl
    , test_macros_macro_in_fundecl_vs_typedef
    , test_macros_macro_redefines_global
    , test_macros_macros
    , test_macros_parse_simple
    , test_macros_parse_elaborate
    , test_macros_parse_intermittent_include
    , test_macros_parse_intermittent_include_conditional
    , test_macros_parse_macro_typedef_scope_multiple
    , test_macros_wrong_source_location
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_macros_macro_ext_binding_dep :: TestCase
test_macros_macro_ext_binding_dep =
    defaultTest "macros/macro_ext_binding_dep"
      & #specExternal .~
          [ "test-artefacts/headers/golden/macros/macro_ext_binding_dep.yaml"
          ]

-- | Macro name-resolution failures are reported at 'Info' by default.
--
-- Regression test for <https://github.com/well-typed/hs-bindgen/issues/2147>:
-- such failures are common in real headers (e.g. preprocessor-only operators),
-- so they must not be warnings by default.
test_macros_macro_resolution_log_level_default :: TestCase
test_macros_macro_resolution_log_level_default =
    testVariant "macros/macro_resolution_log_level" "default"
      & #tracePredicate .~ multiTracePredicate expected (\trace -> case trace of
            MatchUnusable name (UnusableMacroResolutionFailure{})
              | getDefaultLogLevel trace == Info -> Just $ Expected name
            _otherwise -> Nothing
          )
  where
    expected :: [C.DeclName]
    expected = ["macro UNRESOLVED_MACRO"]

-- | 'EnableMacroWarnings' raises macro name-resolution failures to 'Warning'.
--
-- Companion to 'test_macros_macro_resolution_log_level_default'; the predicate
-- insists the (custom) log level is 'Warning'.
test_macros_macro_resolution_log_level_warnings :: TestCase
test_macros_macro_resolution_log_level_warnings =
    testVariant "macros/macro_resolution_log_level" "enable_macro_warnings"
      & #tracePredicate .~ multiTracePredicateCustomLogLevel customLogLevel
          expected (\trace -> case trace of
            MatchUnusable name (UnusableMacroResolutionFailure{})
              | applyCustomLogLevel customLogLevel trace (getDefaultLogLevel trace)
                  == Warning -> Just $ Expected name
            _otherwise -> Nothing
          )
  where
    customLogLevel = getCustomLogLevel [EnableMacroWarnings]

    expected :: [C.DeclName]
    expected = ["macro UNRESOLVED_MACRO"]

test_macros_macro_type_unresolved_tagged :: TestCase
test_macros_macro_type_unresolved_tagged =
    testTraceMulti "macros/macro_type_unresolved_tagged" declsWithMsgs $ \case
      MatchUnusable name@"struct Unparsable" (UnusableParseFailure ParseUnsupportedLongDouble) ->
        Just $ Expected (name, "select-parse-failure")
      MatchSelect name@"macro PTR_UNPARSABLE" (TransitiveDependenciesMissing{}) ->
        Just $ Expected (name, "select-transitive-dependencies-missing")
      MatchUnusable name@"macro PTR_UNPARSABLE" (UnusableMangleNamesFailure{}) ->
        Just $ Expected (name, "select-mangle-names-failure")
      MatchUnusable name@"macro PTR_UNPARSABLE" (UnusableMacroResolutionFailure{}) ->
        Just $ Expected (name, "macro-unresolved-tagged-type")
      MatchUnusable name@"macro PTR_DOES_NOT_EXIST" (UnusableMacroResolutionFailure{}) ->
        Just $ Expected (name, "macro-unresolved-tagged-type")
      MatchUnusable name@"macro DOES_NOT_EXIST" (UnusableMacroResolutionFailure{}) ->
        Just $ Expected (name, "macro-unresolved-tagged-type")
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
        ("struct Unparsable",        "select-parse-failure")
      , ("macro PTR_UNPARSABLE",     "select-transitive-dependencies-missing")
      , ("macro PTR_UNPARSABLE",     "select-mangle-names-failure")
      , ("macro PTR_DOES_NOT_EXIST", "macro-unresolved-tagged-type")
      , ("macro DOES_NOT_EXIST",     "macro-unresolved-tagged-type")
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
          & #selectionPredicate .~ BTrue
          )

test_macros_parse_elaborate :: TestCase
test_macros_parse_elaborate =
    defaultTest "macros/parse/elaborate"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BTrue
          )

test_macros_parse_intermittent_include :: TestCase
test_macros_parse_intermittent_include =
    defaultTest "macros/parse/intermittent_include"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BTrue
          )

test_macros_parse_intermittent_include_conditional :: TestCase
test_macros_parse_intermittent_include_conditional =
    defaultTest "macros/parse/intermittent_include_conditional"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BTrue
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
    declsWithMsgs :: [(C.DeclName, String)]
    declsWithMsgs = [
        ("FOO", "redefinition")
      , ("macro FOO", "conflict")
      ]

test_macros_parse_macro_typedef_scope_multiple :: TestCase
test_macros_parse_macro_typedef_scope_multiple =
    defaultTest "macros/parse/macro_typedef_scope_multiple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BTrue
          )

test_macros_wrong_source_location :: TestCase
test_macros_wrong_source_location =
    defaultTest "macros/wrong_source_location"
      -- Spelling location is only populated correctly on llvm >= 19.1.0;
      -- older toolchains cannot disambiguate anonymous declarations
      -- originating from the same macro expansion.
      & #clangVersion .~ Just (>= (19, 1, 0))
