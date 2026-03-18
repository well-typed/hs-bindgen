-- | Golden tests: comprehensive
--
-- These are larger test cases, possibly coming from external sources (such as
-- reported issues).
--
-- We use 'OmitFieldPrefixes' for all of these tests, partly to have some more
-- tests for skipping field prefixes, and partly because we think this is the way
-- most people should use 'hs-bindgen'.
module Test.HsBindgen.Golden.Comprehensive (testCases) where

import HsBindgen.Config.Internal
import HsBindgen.Frontend.Naming
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
testCases = map omitFieldPrefixes [
      test_comprehensive_c2hsc
    , test_comprehensive_smoke
    ]
  where
    omitFieldPrefixes :: TestCase -> TestCase
    omitFieldPrefixes =
      #onFrontend .~ ( #fieldNamingStrategy .~ OmitFieldPrefixes )

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_comprehensive_c2hsc :: TestCase
test_comprehensive_c2hsc =
    defaultTest "comprehensive/c2hsc"
      & #tracePredicate .~ multiTracePredicate expected trace
  where
    expected :: [CDeclName]
    expected = [
        "ordinary_long_double"
      , "ordinary_long_double_pointer"
      , "ordinary_long_double_array"
      , "ordinary_long_double_pointer_array"
      , "struct ordinary_long_double_struct"
      , "struct ordinary_long_double_pointer_struct"
      , "struct ordinary_long_double_array_struct"
      , "struct ordinary_long_double_pointer_array_struct"
      ]

    trace :: TraceMsg -> Maybe (TraceExpectation CDeclName)
    trace = \case
      MatchDelayed name ParseUnsupportedLongDouble ->
        Just $ Expected name
      _otherwise ->
        Nothing

test_comprehensive_smoke :: TestCase
test_comprehensive_smoke =
    defaultTest "comprehensive/smoke"
      & #cStandard .~ c99
