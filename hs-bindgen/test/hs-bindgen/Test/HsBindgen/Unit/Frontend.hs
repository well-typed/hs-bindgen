module Test.HsBindgen.Unit.Frontend (tests) where

import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Version

import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Config.Internal
import HsBindgen.Frontend
import HsBindgen.Frontend.AST.Decl qualified as C
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

import Test.Common.HsBindgen.Trace.Predicate
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: IO TestResources -> TestTree
tests getTestResources = testGroup "Test.HsBindgen.Unit.Frontend" [
      testGroup "Parse" [
          testParseSequenceNumber getTestResources
        ]
    ]

{-------------------------------------------------------------------------------
  Parse pass tests
-------------------------------------------------------------------------------}

testParseSequenceNumber :: IO TestResources -> TestTree
testParseSequenceNumber getTestResources =
  testWhenClangVersion (>= (20, 1, 0)) "ParseSequenceNumber" $ do
    results <-
      execFrontend
        getTestResources
        c89
        ["test-artefacts" </> "headers"]
        "minimal.h"
        getParseResults
    forM_ results $ \result -> case getParseResultMaybeDecl result of
      Nothing   -> assertFailure $ "parse failed: " ++ show result
      Just decl -> do
        let msg = "no sequence number for declaration " ++ show decl.info.id
        assertBool msg (isJust decl.info.seqNr)

{-------------------------------------------------------------------------------
  Auxiliary functions
-------------------------------------------------------------------------------}

testWhenClangVersion ::
     ((Int, Int, Int) -> Bool)
  -> TestName
  -> Assertion
  -> TestTree
testWhenClangVersion p name assertion
    | isRunnable = testCase  name assertion
    | otherwise  = testGroup name []
  where
    isRunnable :: Bool
    isRunnable = case runtimeClangVersion of
      ClangVersion version  -> p version
      ClangVersionUnknown _ -> False

execFrontend ::
     IO TestResources
  -> CStandard
  -> [FilePath]  -- ^ Include directories
  -> FilePath    -- ^ Header
  -> (FrontendArtefact -> IO a)
  -> IO a
execFrontend getTestResources cStd incDirs header k =
    withTracePredicate noReport defaultTracePredicate $ \tracer -> do
      clangArgs <- getTestClangArgsConfig cStd incDirs <$> getTestResources
      let bootConfig     = BootConfig clangArgs def def
          frontendConfig = def
          backendConfig  = def{ uniqueId = UniqueId "test" }
          config         = BindgenConfig bootConfig frontendConfig backendConfig
          bootTracer     = contramap TraceBoot tracer
          frontendTracer = contramap TraceFrontend tracer
      bootArtefact <- runBoot bootTracer config [header]
      frontendArtefact <- runFrontend frontendTracer frontendConfig bootArtefact
      k frontendArtefact
  where
    noReport :: a -> IO ()
    noReport = const $ pure ()

getParseResults :: FrontendArtefact -> IO [ParseResult Parse]
getParseResults = getCached . (.parse)
