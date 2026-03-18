-- | Golden tests
module Test.HsBindgen.Golden (
    tests
  , allTestCases
  , module Test.HsBindgen.Golden.TestCase
  ) where

import System.Directory (createDirectoryIfMissing)
import Test.Tasty

import Clang.Version

import Test.HsBindgen.Golden.Arrays qualified as Arrays
import Test.HsBindgen.Golden.Attributes qualified as Attributes
import Test.HsBindgen.Golden.BindingSpecs qualified as BindingSpecs
import Test.HsBindgen.Golden.Check.BindingSpec qualified as BindingSpec
import Test.HsBindgen.Golden.Check.FailureBindgen qualified as FailureBindgen
import Test.HsBindgen.Golden.Check.FailureLibclang qualified as FailureLibclang
import Test.HsBindgen.Golden.Check.PP qualified as PP
import Test.HsBindgen.Golden.Check.TH qualified as TH
import Test.HsBindgen.Golden.Comprehensive qualified as Comprehensive
import Test.HsBindgen.Golden.Declarations qualified as Declarations
import Test.HsBindgen.Golden.Documentation qualified as Documentation
import Test.HsBindgen.Golden.EdgeCases qualified as EdgeCases
import Test.HsBindgen.Golden.Functions qualified as Functions
import Test.HsBindgen.Golden.Globals qualified as Globals
import Test.HsBindgen.Golden.Macros qualified as Macros
import Test.HsBindgen.Golden.Manual qualified as Manual
import Test.HsBindgen.Golden.ProgramAnalysis qualified as ProgramAnalysis
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Golden.Types qualified as Types
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testTreeFor getTestResources $
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
      , TestCases "macros"          Macros.testCases
      , TestCases "manual"          Manual.testCases
      , TestCases "programAnalysis" ProgramAnalysis.testCases
      , TestCases "types"           Types.testCases
      ]

-- | All test cases, for use by TH fixture compilation
allTestCases :: [TestCase]
allTestCases = concat [
      Arrays.testCases
    , Attributes.testCases
    , BindingSpecs.testCases
    , Comprehensive.testCases
    , Declarations.testCases
    , Documentation.testCases
    , EdgeCases.testCases
    , Functions.testCases
    , Globals.testCases
    , Macros.testCases
    , Manual.testCases
    , ProgramAnalysis.testCases
    , Types.testCases
    ]

{-------------------------------------------------------------------------------
  Construct tasty test tree
-------------------------------------------------------------------------------}

data TestCaseTree =
    TestCaseSection String [TestCaseTree]
  | TestCases String [TestCase]

testTreeFor :: IO TestResources -> TestCaseTree -> TestTree
testTreeFor getTestResources = goTree
  where
    goTree :: TestCaseTree -> TestTree
    goTree (TestCaseSection label sections) =
        testGroup label $ map goTree sections
    goTree (TestCases label cases) =
        testGroup label $ map goCase cases

    goCase :: TestCase -> TestTree
    goCase test
      | Just versionPred <- test.clangVersion
      , case clangVersion of
          ClangVersion version  -> not (versionPred version)
          ClangVersionUnknown _ -> True
      = testGroup test.name []

      | otherwise
      = case test.outcome of
          Success ->
            withTestOutputDir test.outputDir $ testGroup test.name [
                TH.check          getTestResources test
              , PP.check          getTestResources test
              , BindingSpec.check getTestResources test
              ]
          FailureBindgen ->
            FailureBindgen.check getTestResources test
          FailureLibclang ->
            FailureLibclang.check getTestResources test

    withTestOutputDir :: FilePath -> TestTree -> TestTree
    withTestOutputDir outputDir k =
        withResource
          (createDirectoryIfMissing True outputDir)
          (\_ -> pure ())
          (\_ -> k)
