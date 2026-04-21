-- | Golden tests: documentation
module Test.HsBindgen.Golden.Documentation (testCases) where

import HsBindgen.Doxygen (DoxygenMsg (..))
import HsBindgen.Imports

import Test.Common.HsBindgen.Trace.Patterns (pattern MatchDoxygen)
import Test.Common.HsBindgen.Trace.Predicate (TraceExpectation (..),
                                              multiTracePredicate)
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      defaultTest "documentation/data_kind_pragma"
    , test_documentation_doxygen_docs
    , defaultTest "documentation/javadoc_banner"
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_documentation_doxygen_docs :: TestCase
test_documentation_doxygen_docs =
    testTrace "documentation/doxygen_docs"
      (multiTracePredicate @Text [] $ \case
        MatchDoxygen (DoxygenUnsupported _) -> Just Tolerated
        _otherwise -> Nothing
      )
      & #clangVersion .~ Just (>= (19, 0, 0))
      & #cStandard .~ c99
