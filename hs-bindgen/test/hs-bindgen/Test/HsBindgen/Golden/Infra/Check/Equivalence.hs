module Test.HsBindgen.Golden.Infra.Check.Equivalence (check) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import System.Directory (canonicalizePath, copyFile, createDirectoryIfMissing,
                         doesDirectoryExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, askOption, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

import Clang.Paths (RealPath (..))

import HsBindgen
import HsBindgen.ArtefactM (DirPolicy (..), FilePolicy (..))
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Config.Internal (BaseModuleName (..), BootConfig (..),
                                  FrontendConfig (..), ModuleRenderConfig (..))
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate (Boolean (..), HeaderPathPredicate (..),
                                     SelectionPredicate (..))
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.Library qualified as Library

import Test.Common.HsBindgen.Trace.Predicate (tolerateAll)
import Test.Common.Util.Tasty.Golden (Debug (..))
import Test.HsBindgen.Golden.Infra.Check.Library (Files, compareFiles,
                                                  readFiles)
import Test.HsBindgen.Golden.Infra.TestCase
import Test.HsBindgen.Resources

{-------------------------------------------------------------------------------
  The check
-------------------------------------------------------------------------------}

check :: IO TestResources -> TestCase -> TestTree
check getTestResources test
  | not (isEligible test)
  = testGroup "equivalence" [testCase "skip" (pure ())]
  | otherwise
  = askOption $ \(Debug debug) ->
    testGroup "equivalence" [
      testCase "library-vs-single" $
        withSystemTempDirectory "hs-bindgen-equiv" $ \tmpDir -> do
          testResources <- getTestResources
          let report = if debug then putStrLn else const (pure ())

          originalHeaderDir <- setupLibraryDir testResources test (tmpDir </> "lib")
          unitModule <- computeUnitModule (tmpDir </> "lib") test.inputHeader

          let env = Env {
                  report             = report
                , getTestResources   = getTestResources
                , test               = test
                , tmpDir             = tmpDir
                , libDir             = tmpDir </> "lib"
                , originalHeaderDir  = originalHeaderDir
                , unitModule         = unitModule
                }
          libFiles    <- generateLibrary env
          singleFiles <- generateSingle  env
          compareOutputs libFiles singleFiles
    ]

{-------------------------------------------------------------------------------
  Running the two modes
-------------------------------------------------------------------------------}

data Env = Env {
      report             :: String -> IO ()
    , getTestResources   :: IO TestResources
    , test               :: TestCase
    , tmpDir             :: FilePath
    , libDir             :: FilePath
    , originalHeaderDir  :: FilePath
    , unitModule         :: Hs.ModuleName
    }

-- | Library mode over the one-header library. Empty map on 'NoHeadersFound'.
generateLibrary :: Env -> IO Files
generateLibrary env = do
    let outputDir = env.tmpDir </> "out-lib"
        libTest = env.test
          & #tracePredicate .~ tolerateAll
          & #onBoot %~ (\ob bc -> (ob bc)
              { baseModule = BaseModuleName "Lib"
              , clangArgs  = (ob bc).clangArgs
                  { extraIncludeDirs =
                      -- lib/ first: clang resolves the root header to the
                      -- copy, whose realpath falls under --library.
                      -- Original dir second: sibling #includes resolve there.
                      env.libDir
                        : env.originalHeaderDir
                        : (ob bc).clangArgs.extraIncludeDirs
                  }
              })
          & #onFrontend %~ (\of_ fc -> (of_ fc)
              { selectionPredicate = BIf (SelectHeader FromLibraryHeaders)
              })
    result <- runTestHsBindgenLibrary
      env.report env.getTestResources libTest [env.libDir] outputDir Nothing
    case result of
      Left (BindgenPlanError (Library.NoHeadersFound _)) -> pure Map.empty
      Left err -> assertFailure ("Library mode failed: " ++ show err)
      Right _planned -> do
        exists <- doesDirectoryExist outputDir
        if exists then readFiles outputDir else pure Map.empty

-- | Single-header mode with @--module@ set to the unit's module name, so
-- paths and unique symbols match the library run.
generateSingle :: Env -> IO Files
generateSingle env = do
    let outputDir = env.tmpDir </> "out-single"
        unitModuleText = Text.pack (Hs.moduleNameToString env.unitModule)
        mrc = ModuleRenderConfig { qualifiedStyle = env.test.qualifiedStyle }
        singleTest = env.test
          & #tracePredicate .~ tolerateAll
          & #onBoot %~ (\ob bc -> (ob bc)
              { baseModule = BaseModuleName unitModuleText
              })
    createDirectoryIfMissing True outputDir
    runTestHsBindgenSuccess env.report env.getTestResources singleTest $
      writeBindingsMultiple mrc AllowFileOverwrite CreateOutputDirs outputDir
    exists <- doesDirectoryExist outputDir
    if exists then readFiles outputDir else pure Map.empty

{-------------------------------------------------------------------------------
  Comparison
-------------------------------------------------------------------------------}

-- | Compare the files, skipping two cases that differ by design:
--
-- * @Lib\/Callback.hs@ present: raw function-type instances go there in
--   library mode and stay in the types module in single-header mode.
-- * Empty output: nothing to compare.
compareOutputs :: Files -> Files -> IO ()
compareOutputs libFiles singleFiles
  | Map.member ("Lib" </> "Callback.hs") libFiles = pure ()
  | Map.null libFiles = pure ()
  | otherwise =
      case compareFiles singleFiles libFiles of
        Nothing   -> pure ()
        Just diff ->
          assertFailure $
            "Library mode and single-header mode differ:\n" ++ diff

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Copy the root header into an isolated @--library@ directory.
--
-- Returns the original header's parent directory (kept on @-I@ so sibling
-- @#include@s resolve). We cannot use the golden's directory as @--library@
-- because it holds other headers that would each become a library unit.
setupLibraryDir :: TestResources -> TestCase -> FilePath -> IO FilePath
setupLibraryDir testResources test libDir = do
    let srcHeader =
          testResources.packageRoot </> test.inputDir </> test.inputHeader
        dstHeader = libDir </> test.inputHeader
    createDirectoryIfMissing True (takeDirectory dstHeader)
    copyFile srcHeader dstHeader
    pure (testResources.packageRoot </> test.inputDir
            </> takeDirectory test.inputHeader)

-- | The module name library mode derives from the header path.
-- @smoke.h@ under @--library@ with @--module Lib@ gives @Lib.Smoke@.
computeUnitModule :: FilePath -> FilePath -> IO Hs.ModuleName
computeUnitModule libDir inputHeader = do
    canonicalLibDir <- canonicalizePath libDir
    canonicalHeader <- canonicalizePath (libDir </> inputHeader)
    pure $ Library.moduleNameFromPath
      (Hs.ModuleName "Lib") canonicalLibDir
      (RealPath (Text.pack canonicalHeader))

{-------------------------------------------------------------------------------
  Eligibility
-------------------------------------------------------------------------------}

-- | Ineligible when the two modes differ by design, not by drift:
--
-- * Hash defines: library mode puts them in the vendor module.
-- * Prescriptive spec: its @hsmodule@ matches @Lib@ in library mode and
--   @Lib.Smoke@ in single-header mode, so it applies in one but not the other.
-- * Program slicing: vendor declarations go to a separate module.
-- * Non-default selection predicate: the check sets @FromLibraryHeaders@ and
--   would replace the test's own predicate.
isEligible :: TestCase -> Bool
isEligible test =
       null test.hashDefines
    && isNothing test.specPrescriptive
    && not hasSlicing
    && not hasNonDefaultPredicate
  where
    fc = getTestFrontendConfig test

    hasSlicing = case fc.programSlicing of
      EnableProgramSlicing  -> True
      DisableProgramSlicing -> False

    hasNonDefaultPredicate =
      fc.selectionPredicate /= def
