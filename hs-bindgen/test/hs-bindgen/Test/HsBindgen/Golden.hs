-- | Golden tests
module Test.HsBindgen.Golden (
    tests
  , allTestCases
  , module Test.HsBindgen.Golden.Infra.TestCase
  ) where

import Test.Tasty

import Test.HsBindgen.Golden.Arrays qualified as Arrays
import Test.HsBindgen.Golden.Attributes qualified as Attributes
import Test.HsBindgen.Golden.BindingSpecs qualified as BindingSpecs
import Test.HsBindgen.Golden.Comprehensive qualified as Comprehensive
import Test.HsBindgen.Golden.Declarations qualified as Declarations
import Test.HsBindgen.Golden.Documentation qualified as Documentation
import Test.HsBindgen.Golden.EdgeCases qualified as EdgeCases
import Test.HsBindgen.Golden.Functions qualified as Functions
import Test.HsBindgen.Golden.Globals qualified as Globals
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Golden.Infra.TestCaseTree
import Test.HsBindgen.Golden.Macros qualified as Macros
import Test.HsBindgen.Golden.Macros.Reparse qualified as Macros.Reparse
import Test.HsBindgen.Golden.ProgramAnalysis qualified as ProgramAnalysis
import Test.HsBindgen.Golden.Types qualified as Types
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testTreeFor getTestResources testCaseTree

testCaseTree :: TestCaseTree
testCaseTree =
    TestCaseSection "Test.HsBindgen.Golden" [
        TestCases "arrays"          Arrays.testCases
      , TestCases "attributes"      Attributes.testCases
      , TestCases "bindingSpecs"    BindingSpecs.testCases
      , TestCases "comprehensive"   Comprehensive.testCases
      , TestCases "declarations"    Declarations.testCases
      , TestCases "documentation"   Documentation.testCases
      , TestCases "edgeCases"       EdgeCases.testCases
      , TestCases "functions"       Functions.testCases
      , TestCases "globals"         Globals.testCases
      , TestCaseSection "macros" $ concat [
           TestCaseLeafs Macros.testCases
          , Macros.Reparse.testCases
          ]
      , TestCases "programAnalysis" ProgramAnalysis.testCases
      , TestCases "types"           Types.testCases
      ]

-- | All test cases, for use by TH fixture compilation
allTestCases :: [TestCase]
allTestCases = flatten testCaseTree
