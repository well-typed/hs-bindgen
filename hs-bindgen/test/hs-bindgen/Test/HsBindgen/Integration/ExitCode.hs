module Test.HsBindgen.Integration.ExitCode (tests) where

import Control.Exception (fromException, try)
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
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

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

-- | Test that should successful complete without throwing TraceException
--
testSuccessCase :: IO TestResources -> TestTree
testSuccessCase testResources = testCase "success does not throw" $ do
  let test = defaultTest "functions/simple_func"
      noReport = const $ pure ()

  result <- try $ runTestHsBindgen' noReport testResources test FinalDecls
  case result of
    Right _ -> pure ()
    Left e'
      | Just (TraceException @TraceMsg _)
        <- fromException e' ->
          assertFailure "Expected success but got TraceException"
      | otherwise ->
        assertFailure $ "Unexpected exception: " ++ show e'

-- | Test unresolved #include (issue #1197 scenario)
--
-- Should fail
--
testUnresolvedInclude :: IO TestResources -> TestTree
testUnresolvedInclude testResources = testCase "unresolved include throws TraceException" $ do
  withSystemTempDirectory "hs-bindgen-test" $ \tmpDir -> do
    let tempHeader = tmpDir </> "test-unresolved.h"
    writeFile tempHeader "#include <nonexistent/totally-bogus-header-12345.h>\n"

    let test = (defaultFailingTest "test-unresolved") {
            testInputDir = tmpDir
            -- Tolerate all traces - we want to test TraceException propagation
          , testTracePredicate = customTracePredicate [] $ \_ -> Just Tolerated
          }
        noReport = const $ pure ()

    result <- try $ runTestHsBindgen' noReport testResources test FinalDecls
    case result of
      Left e'
        | Just (TraceException @TraceMsg _)
          <- fromException e' -> pure ()
        | otherwise ->
            assertFailure $ "Expected TraceException but got: " ++ show e'
      Right _ ->
        assertFailure "Expected TraceException for unresolved include but got success"

-- | Test that actual process exit code
--
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

-- Test unresolved #include (the original issue scenario)
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
