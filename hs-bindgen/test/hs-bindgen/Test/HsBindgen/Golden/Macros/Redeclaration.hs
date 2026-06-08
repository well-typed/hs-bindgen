-- | Golden tests: macro redeclaration
module Test.HsBindgen.Golden.Macros.Redeclaration (testCases) where

import Control.Applicative

import HsBindgen.Frontend.Naming
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (DelayedPrepareReparseMsg (PrepareReparseExpansionNotUnique))
import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
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
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_def_undef_def :: TestCase
test_def_undef_def =
    defaultTest_custom "macros/redeclaration/def_undef_def"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [CDeclName]
    expected = ["macro T", "foo", "foo", "bar", "bar"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchDelayed name@"foo" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayed name@"bar" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_different :: TestCase
test_different =
    defaultTest_custom "macros/redeclaration/different"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [CDeclName]
    expected = ["macro T", "foo", "foo", "bar", "bar"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      (matchDiagnosticSpelling "macro redefined" -> Just _diag) ->
        Just $ Tolerated
      MatchDelayed name@"foo" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayed name@"bar" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_identical_semantics :: TestCase
test_identical_semantics =
    defaultTest_custom "macros/redeclaration/identical_semantics"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [CDeclName]
    expected = ["macro T", "foo", "foo", "bar", "bar"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      MatchDelayed name@"foo" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayed name@"bar" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_identical_syntax :: TestCase
test_identical_syntax =
    defaultTest_custom "macros/redeclaration/identical_syntax"
      & #tracePredicate .~ multiTracePredicate_custom expected trace
  where
    expected :: [CDeclName]
    expected = ["macro A", "macro T", "foo", "foo", "bar", "bar"]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchSelect name@"macro A" SelectConflict{} ->
        Just $ Expected name
      MatchSelect name@"macro T" SelectConflict{} ->
        Just $ Expected name
      MatchDelayed name@"foo" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"foo" PrepareReparseExpansionNotUnique{} ->
        Just $ Expected name
      MatchDelayed name@"bar" ParseMacroErrorReparse{} ->
        Just $ Expected name
      MatchDelayedPrepareReparse name@"bar" PrepareReparseExpansionNotUnique{} ->
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

    defaultPredicate :: TraceMsg -> Maybe (TraceExpectation b)
    defaultPredicate = \case
        MatchDelayed _info ParseMacroErrorParse{} ->
          Just Unexpected
        MatchDelayed _info ParseMacroErrorReparse{} ->
          Just Unexpected
        MatchDelayed _info ParseMacroErrorReparseZip{} ->
          Just Unexpected
        MatchImmediatePrepareReparse _ ->
          Just Unexpected
        MatchDelayedPrepareReparse _info _ ->
          Just Unexpected
        _ -> Nothing
