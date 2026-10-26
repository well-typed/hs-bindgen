-- | Golden tests: macros
module Test.HsBindgen.Golden.Macros (
    testCases
  , fallbackTestCases
  ) where

import HsBindgen.Config.ClangArgs
import HsBindgen.Config.Internal
import HsBindgen.Frontend.DeclDB (InactiveReason (..), UnusableReason (..))
import HsBindgen.Frontend.Pass.ApplyMacroFallback.IsPass (MacroFallback (..))
import HsBindgen.Frontend.Pass.Parse.IsPass (EmptyMacros (..))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro.Error (MacroLangFailure (..))
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
    , defaultTest "macros/keyword_params"
    , defaultTest "macros/macro_functions"
    , defaultTest "macros/macro_strings"
    , defaultTest "macros/macro_type_ptr_order"
    , defaultTest "macros/macro_type_ptr_qualifiers"
    , defaultTest "macros/macro_type_void"
    , defaultTest "macros/macro_typedef_struct"
    , defaultTest "macros/macro_types"
    , defaultTest "macros/object_like_as_function_like"
    , defaultTest "macros/parse/first_parse_then_typecheck"
    , defaultTest "macros/parse/macro_typedef_scope"
    , defaultTest "macros/undef"
      -- Bespoke tests
    , test_command_line_uses
    , test_command_line_uses_no_slicing
    , test_command_line_redefined
    , test_command_line_redefined_hash_define
    , test_empty_body
    , test_empty_body_parse
    , test_gnu_variadic
    , test_macro_comma
    , test_macro_ext_binding_dep
    , test_macro_resolution_log_level_default
    , test_macro_resolution_log_level_warnings
    , test_macro_type_unresolved_tagged
    , test_macro_in_fundecl
    , test_macro_in_fundecl_vs_typedef
    , test_macro_redefines_global
    , test_macros
    , test_parse_simple
    , test_parse_elaborate
    , test_parse_intermittent_include
    , test_parse_intermittent_include_conditional
    , test_parse_macro_typedef_scope_multiple
    , test_wrong_source_location
    ]

-- | The tests of the macro fallback
--
-- Not run with the other macro languages: they set the fallback themselves.
fallbackTestCases :: [TestCase]
fallbackTestCases = [
      test_macro_fallback
    , test_macro_fallback_by_decl_name
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

-- | Macros defined by @-D@ options are declarations
--
-- Program slicing selects the ones the header uses.
test_command_line_uses :: TestCase
test_command_line_uses =
    defaultTest "macros/command_line/uses"
      & #onBoot     .~ ( #clangArgs % #argsAfter .~ commandLineUsesArgs )
      & #onFrontend .~ ( #programSlicing .~ EnableProgramSlicing )

-- | Without program slicing, the default predicate does not select macros
-- defined by @-D@ options, and so neither the declarations that use them
test_command_line_uses_no_slicing :: TestCase
test_command_line_uses_no_slicing =
    testVariant "macros/command_line/uses" Nothing "no_slicing"
      & #onBoot         .~ ( #clangArgs % #argsAfter .~ commandLineUsesArgs )
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name (MatchTransMissing _) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["struct S", "macro W"]

commandLineUsesArgs :: [String]
commandLineUsesArgs = ["-DT=int", "-DV=1", "-DF(x)=((x)+1)"]

-- | A header redefines a macro defined by a @-D@ option: a conflict
test_command_line_redefined :: TestCase
test_command_line_redefined =
    defaultTest "macros/command_line/redefined"
      & #onBoot         .~ ( #clangArgs % #argsAfter .~ ["-DT=int"] )
      & #tracePredicate .~ commandLineRedefinedTracePredicate

-- | A header redefines a macro defined by a @#define@ root directive: a
-- conflict
test_command_line_redefined_hash_define :: TestCase
test_command_line_redefined_hash_define =
    testVariant "macros/command_line/redefined" Nothing "hash_define"
      & #hashDefines    .~ [C.HashDefine "T" "int"]
      & #tracePredicate .~ commandLineRedefinedTracePredicate

commandLineRedefinedTracePredicate :: TracePredicate Level TraceMsg
commandLineRedefinedTracePredicate =
    multiTracePredicate declsWithMsgs $ \case
      MatchDiagnosticCategory "Lexical or Preprocessor Issue" ->
        Just $ Expected "redefinition"
      MatchSelect _name SelectConflict{} ->
        Just $ Expected "conflict"
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [String]
    declsWithMsgs = ["redefinition", "conflict"]

-- | An empty macro body is legal C, but by default we do not parse it.
--
-- See <https://github.com/well-typed/hs-bindgen/issues/2246>. Include guards
-- have this shape, so 'DoNotParseEmptyMacros' keeps all four macros from
-- reaching the macro language: they are reported as 'ParseMacroEmpty', and none
-- of them counts towards the dropped-macro summary.
test_empty_body :: TestCase
test_empty_body =
    testTraceMulti "macros/empty_body" (map NotParsed emptyMacroNames) $ \case
      MatchUnusable name (ParseFailed ParseMacroEmpty{}) ->
        Just $ Expected (NotParsed name)
      MatchMacrosDropped{} ->
        Just Unexpected
      _otherwise ->
        Nothing

-- | 'ParseEmptyMacros' passes empty macros on to the macro language.
--
-- Companion to 'test_empty_body': @CExpr@ has no expression to translate, so
-- all four macros now fail like any other macro, and the summary counts them.
-- The @.2.raw@ variant of this test generates bindings for them instead.
test_empty_body_parse :: TestCase
test_empty_body_parse =
    testVariant "macros/empty_body" Nothing "parse_empty_macros"
      & #onFrontend     .~ ( #emptyMacros .~ ParseEmptyMacros )
      & #tracePredicate .~ multiTracePredicate expected (\case
            MatchUnusableReason name (UnusableMacro FailedParse{}) ->
              Just $ Expected (Declined name)
            MatchMacrosDropped n ->
              Just $ Expected (Dropped n)
            _otherwise ->
              Nothing
          )
  where
    expected :: [EmptyBodyTrace]
    expected = Dropped (length emptyMacroNames) : map Declined emptyMacroNames

-- | The macros of the @macros/empty_body@ header
emptyMacroNames :: [C.DeclName]
emptyMacroNames = [
      "macro EMPTY_OBJECT"
    , "macro EMPTY_FUNCTION"
    , "macro EMPTY_FUNCTION_PARAMS"
    , "macro EMPTY_FUNCTION_VARIADIC"
    ]

-- | What we expect to be traced for a macro with an empty replacement list
data EmptyBodyTrace =
    -- | We did not attempt to parse the macro
    NotParsed C.DeclName
    -- | The macro language was given the macro, and declined it
  | Declined C.DeclName
    -- | Summary of the macros that failed to translate
  | Dropped Int
  deriving stock (Eq, Ord, Show)

instance RenderLabel EmptyBodyTrace where
  renderLabel = renderLabel . show

-- | The C99 and the GNU named-variadic parameter lists are different macros.
--
-- @c-expr@ supports neither form, so the bindings only exist in the @raw@
-- variant of this fixture; there, @GNU_VARIADIC(fmt, args...)@ must keep its
-- own spelling, since the C99 form @GNU_VARIADIC(fmt, ...)@ would pass only
-- @fmt@ on. See <https://github.com/well-typed/hs-bindgen/issues/2242>.
test_gnu_variadic :: TestCase
test_gnu_variadic =
    testTraceMulti "macros/gnu_variadic" declsWithMsgs $ \case
      MatchUnusableReason name (UnusableMacro FailedParse{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = [
        "macro C99_VARIADIC"
      , "macro GNU_VARIADIC"
      , "macro GNU_VARIADIC_ONLY"
      ]

-- | A comma in a macro body denotes a tuple, not the C comma operator.
--
-- See <https://github.com/well-typed/hs-bindgen/issues/2182>: C gives macro
-- bodies no meaning of their own, and tuples support the argument-list idiom
-- (@#define ARGS 1, 2@ used as @f(ARGS)@). Consequently, a comma expression
-- does not compose with arithmetic; @ARITH@ is dropped with a parse failure.
test_macro_comma :: TestCase
test_macro_comma =
    testTraceMulti "macros/macro_comma" ["macro ARITH"] $ \case
      MatchUnusableReason name@"macro ARITH" (UnusableMacro FailedParse{}) ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_macro_ext_binding_dep :: TestCase
test_macro_ext_binding_dep =
    defaultTest "macros/macro_ext_binding_dep"
      & #specExternal .~
          [ "test-artefacts/headers/golden/macros/macro_ext_binding_dep.yaml"
          ]

-- | Every macro the macro language gives up on falls back to its raw macro,
-- whatever the stage.
--
-- Program slicing is enabled to show that a fallback macro has no
-- dependencies: @OUTSIDE_DEP@ resolves to a typedef outside the main header,
-- which must not be selected.
test_macro_fallback :: TestCase
test_macro_fallback =
    defaultTest "macros/macro_fallback"
      & #onFrontend     .~ ( ( #macroFallback  .~ MacroFallback BTrue )
                           . ( #emptyMacros    .~ ParseEmptyMacros )
                           . ( #programSlicing .~ EnableProgramSlicing )
                           )
      & #tracePredicate .~ macroFallbackTracePredicate [
            ("macro PARSE_FAILURE",     FailedStageParse)
          , ("macro RESOLVE_FAILURE",   FailedStageResolve)
          , ("macro TYPECHECK_FAILURE", FailedStageTypecheck)
          , ("macro TYPE_LIKE",         FailedStageTypecheck)
          , ("macro CASCADE",           FailedStageTypecheck)
          , ("macro OUTSIDE_DEP",       FailedStageTypecheck)
          , ("macro CONCAT",            FailedStageParse)
          , ("macro LOG",               FailedStageParse)
          , ("macro LOG_NAMED",         FailedStageParse)
          , ("macro EMPTY",             FailedStageParse)
          ]
          []

-- | The fallback predicate narrows which macros fall back; the others are
-- dropped as without fallback, with the reason of their stage.
--
-- @EMPTY@ is not passed to the macro language (no @--parse-empty-macros@), so
-- it is neither.
test_macro_fallback_by_decl_name :: TestCase
test_macro_fallback_by_decl_name =
    testVariant "macros/macro_fallback" Nothing "by_decl_name"
      & #onFrontend     .~ ( #macroFallback .~ MacroFallback
                               (BIf $ SelectDecl $ DeclNameMatches
                                  "^macro (PARSE_FAILURE|TYPE_LIKE)$")
                           )
      & #tracePredicate .~ macroFallbackTracePredicate
          [ ("macro PARSE_FAILURE", FailedStageParse)
          , ("macro TYPE_LIKE",     FailedStageTypecheck)
          ]
          [ ("macro RESOLVE_FAILURE",   FailedStageResolve)
          , ("macro TYPECHECK_FAILURE", FailedStageTypecheck)
          , ("macro CASCADE",           FailedStageTypecheck)
          , ("macro OUTSIDE_DEP",       FailedStageTypecheck)
          , ("macro CONCAT",            FailedStageParse)
          , ("macro LOG",               FailedStageParse)
          , ("macro LOG_NAMED",         FailedStageParse)
          ]

-- | Expect exactly the given macros to fall back, with their summary, and
-- exactly the given macros the macro language gave up on to be dropped
macroFallbackTracePredicate ::
     [(C.DeclName, FailedStage)]
     -- ^ Fallback macros
  -> [(C.DeclName, FailedStage)]
     -- ^ Dropped macros
  -> TracePredicate Level TraceMsg
macroFallbackTracePredicate fallbacks dropped =
    multiTracePredicate expected $ \case
      MatchMacroFallback name failure ->
        Just $ Expected $ FellBack name (failedStage failure)
      MatchMacrosFallback n ->
        Just $ Expected $ FallbackSummary n
      MatchUnusableReason name reason
        | Just stage <- droppedStage reason ->
            Just $ Expected $ Rejected name stage
      _otherwise ->
        Nothing
  where
    expected :: [MacroFallbackTrace]
    expected = concat [
          [FallbackSummary (length fallbacks)]
        , map (uncurry FellBack) fallbacks
        , map (uncurry Rejected) dropped
        ]

-- | What we expect to be traced for a macro the macro language gave up on
data MacroFallbackTrace =
    FellBack C.DeclName FailedStage
  | FallbackSummary Int
  | Rejected C.DeclName FailedStage
  deriving stock (Eq, Ord, Show)

instance RenderLabel MacroFallbackTrace where
  renderLabel = renderLabel . show

data FailedStage =
    FailedStageParse
  | FailedStageResolve
  | FailedStageTypecheck
  deriving stock (Eq, Ord, Show)

failedStage :: MacroLangFailure -> FailedStage
failedStage = \case
    FailedParse{}     -> FailedStageParse
    FailedResolve{}   -> FailedStageResolve
    FailedTypecheck{} -> FailedStageTypecheck

-- | The stage of a macro the macro language gave up on, by its unusable reason
droppedStage :: UnusableReason -> Maybe FailedStage
droppedStage = \case
    UnusableMacro failure -> Just (failedStage failure)
    _otherwise            -> Nothing

-- | Macro name-resolution failures are reported at 'Info' by default.
--
-- Regression test for <https://github.com/well-typed/hs-bindgen/issues/2147>:
-- such failures are common in real headers (e.g. preprocessor-only operators),
-- so they must not be warnings by default.
test_macro_resolution_log_level_default :: TestCase
test_macro_resolution_log_level_default =
    testVariant "macros/macro_resolution_log_level" Nothing "default"
      & #tracePredicate .~ multiTracePredicate expected (\trace -> case trace of
            MatchUnusableReason name (UnusableMacro FailedResolve{})
              | getDefaultLogLevel trace == Info -> Just $ Expected name
            _otherwise -> Nothing
          )
  where
    expected :: [C.DeclName]
    expected = ["macro UNRESOLVED_MACRO"]

-- | 'EnableMacroWarnings' raises macro name-resolution failures to 'Warning'.
--
-- Companion to 'test_macro_resolution_log_level_default'; the predicate
-- insists the (custom) log level is 'Warning'.
test_macro_resolution_log_level_warnings :: TestCase
test_macro_resolution_log_level_warnings =
    testVariant "macros/macro_resolution_log_level" Nothing "enable_macro_warnings"
      & #tracePredicate .~ multiTracePredicateCustomLogLevel customLogLevel
          expected (\trace -> case trace of
            MatchUnusableReason name (UnusableMacro FailedResolve{})
              | applyCustomLogLevel customLogLevel trace (getDefaultLogLevel trace)
                  == Warning -> Just $ Expected name
            MatchMacrosDropped{} -> Just Tolerated
            _otherwise -> Nothing
          )
  where
    customLogLevel = getCustomLogLevel [EnableMacroWarnings]

    expected :: [C.DeclName]
    expected = ["macro UNRESOLVED_MACRO"]

test_macro_type_unresolved_tagged :: TestCase
test_macro_type_unresolved_tagged =
    testTraceMulti "macros/macro_type_unresolved_tagged" declsWithMsgs $ \case
      MatchUnusable name@"struct Unparsable"
        (ParseFailed (ParseUnsupportedFloatType UnsupportedLongDouble)) ->
        Just $ Expected (name, "select-parse-failure")
      MatchSelect name@"macro PTR_UNPARSABLE" (TransitiveDependenciesMissing{}) ->
        Just $ Expected (name, "select-transitive-dependencies-missing")
      MatchUnusableReason name@"macro PTR_UNPARSABLE" (UnusableMangleNames{}) ->
        Just $ Expected (name, "select-mangle-names-failure")
      MatchUnusableReason name@"macro PTR_UNPARSABLE" (UnusableMacro FailedResolve{}) ->
        Just $ Expected (name, "macro-unresolved-tagged-type")
      MatchUnusableReason name@"macro PTR_DOES_NOT_EXIST" (UnusableMacro FailedResolve{}) ->
        Just $ Expected (name, "macro-unresolved-tagged-type")
      MatchUnusableReason name@"macro DOES_NOT_EXIST" (UnusableMacro FailedResolve{}) ->
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

test_macro_in_fundecl :: TestCase
test_macro_in_fundecl =
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

test_macro_in_fundecl_vs_typedef :: TestCase
test_macro_in_fundecl_vs_typedef =
    testTraceMulti "macros/macro_in_fundecl_vs_typedef" declsWithMsgs $ \case
      MatchDelayed name ParsePotentialDuplicateSymbol{} ->
        Just $ Expected name
      _otherwise ->
        Nothing
  where
    declsWithMsgs :: [C.DeclName]
    declsWithMsgs = ["quux1", "quux2", "wam1", "wam2"]

test_macro_redefines_global :: TestCase
test_macro_redefines_global =
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

test_macros :: TestCase
test_macros =
    defaultTest "macros/macros"
      & #cStandard .~ c23

test_parse_simple :: TestCase
test_parse_simple =
    defaultTest "macros/parse/simple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromAllHeaders)
          )

test_parse_elaborate :: TestCase
test_parse_elaborate =
    defaultTest "macros/parse/elaborate"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromAllHeaders)
          )

test_parse_intermittent_include :: TestCase
test_parse_intermittent_include =
    defaultTest "macros/parse/intermittent_include"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromAllHeaders)
          )

test_parse_intermittent_include_conditional :: TestCase
test_parse_intermittent_include_conditional =
    defaultTest "macros/parse/intermittent_include_conditional"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromAllHeaders)
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

test_parse_macro_typedef_scope_multiple :: TestCase
test_parse_macro_typedef_scope_multiple =
    defaultTest "macros/parse/macro_typedef_scope_multiple"
      & #onFrontend .~ (\cfg -> cfg
          & #selectionPredicate .~ BIf (SelectHeader FromAllHeaders)
          )

test_wrong_source_location :: TestCase
test_wrong_source_location =
    defaultTest "macros/wrong_source_location"
      -- Spelling location is only populated correctly on llvm >= 19.1.0;
      -- older toolchains cannot disambiguate unnamed declarations
      -- originating from the same macro expansion.
      & #clangVersion .~ Just (>= (19, 1, 0))
