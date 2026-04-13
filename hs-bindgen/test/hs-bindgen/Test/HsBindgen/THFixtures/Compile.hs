-- | Compilation of TH fixture modules via direct @ghc@ or @haddock@ invocation
--
module Test.HsBindgen.THFixtures.Compile (
    setupBatchCompile
  ) where

import Data.Map.Strict (Map)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.HsBindgen.Fixtures.Utils

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Set up compilation (or haddock generation) of TH fixtures
--
-- Uses direct GHC compilation. When @haddockDir@ is @'Just' dir@, generates
-- browsable Haddock documentation via @haddock --ghc --html@.
--
setupBatchCompile
  :: FilePath            -- ^ Repo root (parent of hs-bindgen/)
  -> Maybe FilePath      -- ^ Haddock output directory, or 'Nothing' for compile-only
  -> [(String, String)]  -- ^ (test name, module content)
  -> IO (Map String (Either String ()))
setupBatchCompile repoRoot mHaddockDir cases =
    withSystemTempDirectory "hs-bindgen-th-fixtures" $ \tmpDir -> do
      -- Write each module into its own subdirectory (all named Example,
      -- so each needs a separate directory for GHC's -i search path)
      mapM_ (writeTestModule tmpDir) cases

      (includeDirs, envFile) <- setupFixtureEnv repoRoot

      let ghcOpts = sharedGhcOptions ++ ["-Wno-unused-imports"]
          jobs =
            [ ( "th-fixture-" ++ sanitizeLibName name
              , tmpDir </> sanitizeLibName name
              , ["Example"]
              , []
              )
            | (name, _) <- cases
            ]

      runDirectGhcBuild tmpDir envFile ghcOpts includeDirs mHaddockDir jobs

{-------------------------------------------------------------------------------
  Module writing
-------------------------------------------------------------------------------}

-- | Write a test module into its subdirectory
writeTestModule :: FilePath -> (String, String) -> IO ()
writeTestModule tmpDir (name, content) = do
    let subDir = tmpDir </> sanitizeLibName name
    createDirectoryIfMissing True subDir
    writeFile (subDir </> "Example.hs") content
