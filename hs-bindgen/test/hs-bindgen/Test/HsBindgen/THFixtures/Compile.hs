-- | GHC compilation helper for TH fixture testing
--
-- This module provides functionality to compile generated TH modules
-- with GHC to verify they produce valid Haskell code.
--
module Test.HsBindgen.THFixtures.Compile (
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
  :: FilePath -- ^ Package root (hs-bindgen directory)
  -> FilePath -- ^ Output directory for build artifacts
  -> FilePath -- ^ Path to the Haskell file to compile
  -> IO (Either String ())
compileFile pkgRoot outputDir modulePath = do
  let examplesDir = pkgRoot </> "examples"
      goldenDir   = pkgRoot </> "examples" </> "golden"

  -- Run cabal exec from hs-bindgen directory (like compile-fixtures.sh does)
  -- This ensures cabal uses the hs-bindgen package environment
  let shellCmd = unwords
        [ "cd", pkgRoot, "&&"
        , "cabal", "exec", "--", "ghc"
        , "-c"
        , "-fforce-recomp"
        , "-Wall"
        , "-Wincomplete-uni-patterns"
        , "-Wincomplete-record-updates"
        , "-Wmissing-exported-signatures"
        , "-Widentities"
        , "-Wredundant-constraints"
        , "-Wpartial-fields"
        , "-Wcpp-undef"
        , "-Wno-unused-matches"
        , "-outputdir", outputDir
        -- Use -package-id to expose the in-place hs-bindgen package
        -- (it's marked as hidden by default in cabal exec environment)
        , "-package-id", "hs-bindgen-0.1.0-inplace"
        , "-package", "hs-bindgen-runtime"
        , "-package", "c-expr-runtime"
        , "-package", "optics"
        , "-optc", "-I" ++ examplesDir
        , "-optc", "-I" ++ goldenDir
        , "-optc", "-std=gnu2x"
        , "-optc", "-Wno-deprecated-declarations"
        , "-optc", "-Wno-attributes"
        , modulePath
        ]

  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode "sh" ["-c", shellCmd] ""

  return $ case exitCode of
      ExitSuccess   -> Right ()
      ExitFailure _ -> Left stderr
