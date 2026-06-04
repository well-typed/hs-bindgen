-- | Golden tests: macro reparsing
module Test.HsBindgen.Golden.Macros.Reparse (testCases) where

import Control.Applicative

import HsBindgen.Frontend.Pass.Select.IsPass
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace.Patterns
import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Golden.Infra.TestCaseTree
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCaseTree]
testCases = [
      TestCaseLeaf test
    , TestCaseLeaf test_arithmetic_types
    , TestCaseLeaf test_cref_attributes
    , TestCaseLeaf test_defer_wrong
    , TestCaseLeaf test_functions
    , TestCaseLeaf test_gnu_attributes
    , TestCases "nesting" [
          defaultTest_nesting "macros/reparse/nesting"
        , defaultTest_nesting "macros/reparse/nesting/struct_in_struct"
        , defaultTest_nesting "macros/reparse/nesting/struct_in_typedef"
        , defaultTest_nesting "macros/reparse/nesting/struct_in_union"
        , defaultTest_nesting "macros/reparse/nesting/struct_in_variable"
        , defaultTest_nesting "macros/reparse/nesting/union_in_struct"
        , defaultTest_nesting "macros/reparse/nesting/union_in_typedef"
        , defaultTest_nesting "macros/reparse/nesting/union_in_union"
        , defaultTest_nesting "macros/reparse/nesting/union_in_variable"
        ]
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test :: TestCase
test =
    defaultTest "macros/reparse"
        -- `bool` is a keyword.
      & #clangVersion   .~ Just (>= (15, 0, 0))
        -- `bool` is a keyword.
      & #cStandard      .~ c23

test_arithmetic_types :: TestCase
test_arithmetic_types =
    defaultTest "macros/reparse/arithmetic_types"
      & #tracePredicate .~ multiTracePredicate declsWithMsgs (\case
            MatchSelect name@"f29" (SelectParseFailure ParseUnsupportedLongDouble) ->
              Just $ Expected name
            _otherwise ->
              Nothing
          )
  where
    declsWithMsgs = ["f29"]

test_cref_attributes :: TestCase
test_cref_attributes =
    defaultTest "macros/reparse/cref_attributes"
      & #cStandard      .~ c23

test_defer_wrong :: TestCase
test_defer_wrong =
    defaultTest "macros/reparse/defer_wrong"

test_functions :: TestCase
test_functions =
    defaultTest "macros/reparse/functions"
      -- C99 required for inline functions
      & #cStandard .~ c99

test_gnu_attributes :: TestCase
test_gnu_attributes =
    defaultTest "macros/reparse/gnu_attributes"
      & #cStandard      .~ c23

{-------------------------------------------------------------------------------
  Nesting
-------------------------------------------------------------------------------}

defaultTest_nesting :: FilePath -> TestCase
defaultTest_nesting path =
    defaultTest path
      & #tracePredicate .~ defaultTracePredicate_nesting

defaultTracePredicate_nesting :: TracePredicate Level TraceMsg
defaultTracePredicate_nesting = multiTracePredicate_nesting @() [] (const Nothing)

multiTracePredicate_nesting :: forall b.
     (Ord b, RenderLabel b)
  => [b]
  -> (TraceMsg -> Maybe (TraceExpectation b))
  -> TracePredicate Level TraceMsg
multiTracePredicate_nesting expected predicate = multiTracePredicate expected predicate'
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
        _ -> Nothing
