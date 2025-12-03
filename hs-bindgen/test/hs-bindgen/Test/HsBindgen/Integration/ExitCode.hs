module Test.HsBindgen.Integration.ExitCode (tests) where

import Control.Monad (void)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Common.HsBindgen.TracePredicate
import Test.HsBindgen.Golden.TestCase
import Test.HsBindgen.Resources
import Test.Tasty
import Test.Tasty.HUnit

import HsBindgen.Artefact (Artefact (..))

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests testResources = testGroup "Integration.ExitCode" [
    testSuccessCase testResources
  , testUnresolvedInclude testResources
  , testSuccessCaseProcess testResources
  , testUnresolvedIncludeProcess
  ]

-- | Test that should complete successfully
testSuccessCase :: IO TestResources -> TestTree
testSuccessCase testResources = testCase "success does not throw" $ do
  let test = defaultTest "functions/simple_func"
      noReport = const $ pure ()
  void $ runTestHsBindgenSuccess noReport testResources test FinalDecls

-- | Test unresolved #include (issue #1197 scenario)
--
-- Should fail
testUnresolvedInclude :: IO TestResources -> TestTree
testUnresolvedInclude testResources = testCase "unresolved include throws exception" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    let tempHeader = tmpDir </> "test-unresolved.h"
    writeFile tempHeader "#include <nonexistent/totally-bogus-header-12345.h>\n"

    let test = (defaultFailingTest "test-unresolved") {
            testInputDir = tmpDir
            -- Tolerate all traces - we want to test exception propagation
          , testTracePredicate = customTracePredicate [] $ \_ -> Just Tolerated
          }
        noReport = const $ pure ()

    void $ runTestHsBindgenFailure noReport testResources test FinalDecls

-- | Test that actual process exit code
testSuccessCaseProcess :: IO TestResources -> TestTree
testSuccessCaseProcess testResources = testCase "success returns exit code 0" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    root <- getTestPackageRoot testResources
    let headerPath = root </> "examples/golden/functions/simple_func.h"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--create-output-dirs"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , headerPath
                                               ]
                                               ""
    exitCode @?= ExitSuccess

-- | Test unresolved #include (the original issue scenario)
testUnresolvedIncludeProcess :: TestTree
testUnresolvedIncludeProcess = testCase "unresolved include returns non-zero" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    -- Create a temporary header with unresolved include
    let tempHeader = tmpDir </> "test-unresolved-include.h"
    writeFile tempHeader "#include <nonexistent/totally-bogus-header-12345.h>\n"
    (exitCode, _, _) <- readProcessWithExitCode "hs-bindgen-cli"
                                               [ "preprocess"
                                               , "--hs-output-dir"
                                               , tmpDir
                                               , tempHeader
                                               ]
                                               ""
    exitCode @?= ExitFailure 1
