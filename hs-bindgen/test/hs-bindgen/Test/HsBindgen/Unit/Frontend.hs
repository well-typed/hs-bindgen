module Test.HsBindgen.Unit.Frontend (tests) where

import Data.List
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

import Clang.Version

import HsBindgen.Boot
import HsBindgen.Cache
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Frontend
import HsBindgen.Frontend.Pass.Parse.IsPass
import HsBindgen.Frontend.Pass.Parse.Result
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro
import HsBindgen.Macro qualified as Macro
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
          testParseSourceOrder getTestResources
        ]
    ]

{-------------------------------------------------------------------------------
  Parse pass tests
-------------------------------------------------------------------------------}

testParseSourceOrder :: IO TestResources -> TestTree
testParseSourceOrder getTestResources =
  testWhenClangVersion (>= (20, 1, 0)) "ParseSourceOrder" $ do
    results <-
      execFrontend
        getTestResources
        c89
        ["test-artefacts" </> "headers" </> "golden" </> "macros" </> "parse"]
        "elaborate.h"
        getParseResults
    declNamesWithSourceOrderIndex <- forM results $ \result ->
      case getParseResultMaybeDecl result of
        Nothing   -> assertFailure $ "parse failed: " ++ show result
        Just decl -> do
          case decl.info.sourceOrderIndex of
            Nothing ->
              panicPure $
                "no source-order index for declaration " ++ show decl.info.id
            Just idx ->
              pure (show $ prettyForTrace decl.info.id, idx)
    -- 'getParseResults' returns declarations in sequence order (libclang's
    -- visitation order: macros first, then the rest).
    let declNamesSequenceOrderActual :: [String]
        declNamesSequenceOrderActual = map fst declNamesWithSourceOrderIndex
        declNamesSequenceOrderExpected :: [String]
        declNamesSequenceOrderExpected = [
             "'macro OUTER_A'"
           , "'macro INNER_A'"
           , "'macro INNER_B'"
           , "'macro OUTER_B'"
           , "'macro OUTER_C'"
           , "'outer_int'"
           , "'inner_int'"
           ]
    assertEqual "sequence order"
      declNamesSequenceOrderExpected
      declNamesSequenceOrderActual
    -- Sorting by the source-order index recovers source order.
    let declNamesSourceOrderActual :: [String]
        declNamesSourceOrderActual =
          map fst $ sortOn snd declNamesWithSourceOrderIndex
        declNamesSourceOrderExpected :: [String]
        declNamesSourceOrderExpected = [
            "'macro OUTER_A'"
          , "'outer_int'"
          , "'macro INNER_A'"
          , "'macro INNER_B'"
          , "'inner_int'"
          , "'macro OUTER_B'"
          , "'macro OUTER_C'"
          ]
    assertEqual "source order"
      declNamesSourceOrderExpected
      declNamesSourceOrderActual

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
  -> (FrontendArtefact CExpr -> IO a)
  -> IO a
execFrontend getTestResources cStdStr incDirs header k =
    withTracePredicate noReport defaultTracePredicate $ \tracer -> do
      clangArgs <- getTestClangArgsConfig cStdStr incDirs <$> getTestResources
      let bootConfig     = BootConfig clangArgs def def
          frontendConfig = def
          backendConfig  = def{ uniqueId = UniqueId "test" }
          config         = BindgenConfig bootConfig frontendConfig backendConfig
          bootTracer     = contramap TraceBoot tracer
          frontendTracer = contramap TraceFrontend tracer
      bootArtefact     <- runBoot bootTracer (pure . Macro.cExpr) config
                            [C.DirectiveHashInclude header]
      frontendArtefact <- runFrontend frontendTracer frontendConfig bootArtefact
      k frontendArtefact
  where
    noReport :: a -> IO ()
    noReport = const $ pure ()

getParseResults :: FrontendArtefact CExpr -> IO [ParseResult CExpr Parse]
getParseResults = getCached . (.parse)
