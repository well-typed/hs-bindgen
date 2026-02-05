-- | GHC compilation helper for TH fixture testing
--
-- This module provides functionality to compile generated TH modules
-- with GHC to verify they produce valid Haskell code.
--
module Test.THFixtures.Compile (
    compileThModule
  ) where

import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)

{-------------------------------------------------------------------------------
  Compilation
-------------------------------------------------------------------------------}

-- | Compile a TH module and return the result
--
-- Creates a temporary directory, writes the module content, and compiles
-- it with GHC through cabal exec.
--
compileThModule ::
       FilePath -- ^ Package root
    -> String   -- ^ Test name (for temp directory naming)
    -> String   -- ^ Module content
    -> IO (Either String ())
compileThModule pkgRoot testName moduleContent =
  withSystemTempDirectory ("th-fixture-" ++ sanitizeName testName) $ \tmpDir -> do
      let modulePath = tmpDir </> "Example.hs"
      writeFile modulePath moduleContent
      compileFile pkgRoot tmpDir modulePath

-- | Sanitize test name for use in file paths
sanitizeName :: String -> String
sanitizeName = map sanitizeChar
  where
    sanitizeChar '/' = '-'
    sanitizeChar c   = c

-- | Compile a single Haskell file with GHC
compileFile
  :: FilePath -- ^ Package root
  -> FilePath -- ^ Output directory for build artifacts
  -> FilePath -- ^ Path to the Haskell file to compile
  -> IO (Either String ())
compileFile pkgRoot outputDir modulePath = do
  let examplesDir = pkgRoot </> "examples"
      goldenDir   = pkgRoot </> "examples" </> "golden"

  -- Use cabal exec to get the correct GHC environment with packages
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "cabal"
      [ "exec", "--", "ghc"
      -- Compile only, no linking
      , "-c"
      -- Always recompile
      , "-fforce-recomp"
      -- Warnings (match compile-fixtures.sh)
      , "-Wall"
      , "-Wincomplete-uni-patterns"
      , "-Wincomplete-record-updates"
      , "-Wmissing-exported-signatures"
      , "-Widentities"
      , "-Wredundant-constraints"
      , "-Wpartial-fields"
      , "-Wcpp-undef"
      , "-Wno-unused-matches"
      -- Output directory for build artifacts
      , "-outputdir", outputDir
      -- Required packages
      , "-package", "hs-bindgen"
      , "-package", "hs-bindgen-runtime"
      , "-package", "c-expr-runtime"
      , "-package", "optics"
      -- C compiler flags
      , "-optc", "-I" ++ examplesDir
      , "-optc", "-I" ++ goldenDir
      , "-optc", "-std=gnu2x"
      , "-optc", "-Wno-deprecated-declarations"
      , "-optc", "-Wno-attributes"
      -- The file to compile
      , modulePath
      ] ""

  return $ case exitCode of
      ExitSuccess   -> Right ()
      ExitFailure _ -> Left stderr
