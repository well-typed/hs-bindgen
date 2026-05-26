-- | Golden tests: macro reparsing
module Test.HsBindgen.Golden.Macros.Reparse (testCases) where

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
      test
    , test_arithmetic_types
    , test_cref_attributes
    , test_defer_wrong
    , test_functions
    , test_gnu_attributes
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
