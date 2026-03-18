-- | Golden tests: documentation
module Test.HsBindgen.Golden.Documentation (testCases) where

import HsBindgen.Imports

import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Test cases
-------------------------------------------------------------------------------}

testCases :: [TestCase]
testCases = [
      defaultTest "documentation/data_kind_pragma"
    , test_documentation_doxygen_docs
    ]

{-------------------------------------------------------------------------------
  Individual test definitions
-------------------------------------------------------------------------------}

test_documentation_doxygen_docs :: TestCase
test_documentation_doxygen_docs =
    defaultTest "documentation/doxygen_docs"
      & #clangVersion .~ Just (>= (19, 0, 0))
      & #cStandard .~ c99
