-- | Golden tests: macro redeclaration
module Test.HsBindgen.Golden.Macros.Redeclaration (testCases) where

import Control.Applicative

import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg (PrepareReparseExpansionNotUnique))
import HsBindgen.Frontend.Pass.ReparseMacroExpansions.IsPass.Msg
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Golden.Infra.TestCaseTree

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCaseTree]
testCases = [
      TestCaseLeaf test_def_undef_def
    , TestCaseLeaf test_different
    , TestCaseLeaf test_identical_semantics
    , TestCaseLeaf test_identical_syntax
    , TestCaseLeaf test_same_line_tag_field
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_def_undef_def :: TestCase
test_def_undef_def =
    defaultTest_custom "macros/redeclaration/def_undef_def"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [C.DeclName]
    expected = ["macro T", "foo", "foo", "foo", "bar", "bar", "bar"]

    -- NOTE: though messages related to reparsing are most often info-level or
    -- below, this predicate matches on all reparse-related trace messages so
    -- that we can precisely assert that the messages make sense
    trace :: TraceMsg -> Maybe (TraceExpectation C.DeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_different :: TestCase
test_different =
    defaultTest_custom "macros/redeclaration/different"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [C.DeclName]
    expected = ["macro T", "foo", "foo", "foo", "bar", "bar", "bar"]

    -- NOTE: though messages related to reparsing are most often info-level or
    -- below, this predicate matches on all reparse-related trace messages so
    -- that we can precisely assert that the messages make sense
    trace :: TraceMsg -> Maybe (TraceExpectation C.DeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_identical_semantics :: TestCase
test_identical_semantics =
    defaultTest_custom "macros/redeclaration/identical_semantics"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [C.DeclName]
    expected = ["macro T", "foo", "foo", "foo", "bar", "bar", "bar"]

    -- NOTE: though messages related to reparsing are most often info-level or
    -- below, this predicate matches on all reparse-related trace messages so
    -- that we can precisely assert that the messages make sense
    trace :: TraceMsg -> Maybe (TraceExpectation C.DeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_identical_syntax :: TestCase
test_identical_syntax =
    defaultTest_custom "macros/redeclaration/identical_syntax"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [C.DeclName]
    expected = ["macro A", "macro T", "foo", "foo", "foo", "bar", "bar", "bar"]

    -- NOTE: though messages related to reparsing are most often info-level or
    -- below, this predicate matches on all reparse-related trace messages so
    -- that we can precisely assert that the messages make sense
    trace :: TraceMsg -> Maybe (TraceExpectation C.DeclName)
    trace = \case
      MatchSelect name@"macro A" SelectConflict{} ->
        Just $ Expected name
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"foo" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" (ReparseMacroExpansionUnknownType "T") ->
        Just $ Expected name
      MatchDelayedReparseMacroExpansions name@"bar" ReparseMacroExpansionsLanC{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

-- | A tag macro expansion must not leak into a same-line field.
--
-- @A@ (redefined, hence conflicting) expands to the struct tag; @B@ expands to
-- the field type. Only @B@ belongs to the field's reparse info, so no
-- reparse-related messages should be emitted for the struct. The default
-- predicate flags any such message as unexpected, so a regression of the
-- same-line mis-attribution fails this test.
test_same_line_tag_field :: TestCase
test_same_line_tag_field =
    defaultTest_custom "macros/redeclaration/same_line_tag_field"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [C.DeclName]
    expected = ["macro A"]

    trace :: TraceMsg -> Maybe (TraceExpectation C.DeclName)
    trace = \case
      MatchSelect name@"macro A" SelectConflict{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

{-------------------------------------------------------------------------------
  Custom
-------------------------------------------------------------------------------}

defaultTest_custom :: FilePath -> TestCase
defaultTest_custom path =
    defaultTest path
      & #tracePredicate .~ defaultTracePredicate_custom

defaultTracePredicate_custom :: TracePredicate Level TraceMsg
defaultTracePredicate_custom = multiTracePredicate_custom @() [] (const Nothing)

multiTracePredicate_custom :: forall b.
     (Ord b, RenderLabel b)
  => [b]
  -> (TraceMsg -> Maybe (TraceExpectation b))
  -> TracePredicate Level TraceMsg
multiTracePredicate_custom expected predicate = multiTracePredicate expected predicate'
  where
    predicate' x = predicate x <|> defaultPredicate x

    -- NOTE: though messages related to reparsing are most often info-level or
    -- below, this predicate marks all reparse-related trace messages as
    -- unexpected so that we can precisely test every message
    defaultPredicate :: TraceMsg -> Maybe (TraceExpectation b)
    defaultPredicate = \case
        -- Parse (macro)
        MatchDelayed _info ParseMacroErrorParse{} ->
          Just Unexpected
        -- PrepareReparse
        MatchImmediatePrepareReparse _ ->
          Just Unexpected
        MatchDelayedPrepareReparse _info _ ->
          Just Unexpected
        -- ReparseMacroExpansions
        MatchImmediateReparseMacroExpansions _ ->
          Just Unexpected
        MatchDelayedReparseMacroExpansions _info _ ->
          Just Unexpected
        _ -> Nothing
