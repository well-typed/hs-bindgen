-- | Compilation of PP fixture modules via direct @ghc@ or @haddock@ invocation
--
module Test.HsBindgen.PPFixtures.Compile (
    setupPPBatchCompile
  , detectModules
  ) where

import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Traversable (forM)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (dropExtension, makeRelative, pathSeparator,
                        takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)

import Test.HsBindgen.Fixtures.Utils

{-------------------------------------------------------------------------------
  Setup
-------------------------------------------------------------------------------}

-- | Set up compilation (or haddock generation) of PP fixtures
--
-- Uses direct GHC compilation. When @haddockDir@ is @'Just' dir@, generates
-- browsable Haddock documentation via @haddock --ghc --html@.
--
setupPPBatchCompile
  :: FilePath
     -- ^ Repo root (parent of hs-bindgen/)
  -> Maybe FilePath
     -- ^ Haddock output directory, or 'Nothing' for compile-only
  -> [(String, FilePath, Bool)]
     -- ^ (test name, absolute fixture dir, include @-optc-Werror@?)
  -> IO (Map String (Either String ()))
setupPPBatchCompile repoRoot mHaddockDir cases =
    withSystemTempDirectory "hs-bindgen-pp-fixtures" $ \tmpDir -> do
      (includeDirs, envFile) <- setupFixtureEnv repoRoot

      -- Detect modules in each fixture directory
      casesWithModules <- forM cases $ \(name, fixtureDir, optcWerror) -> do
        mods <- detectModules fixtureDir
        return (name, fixtureDir, mods, optcWerror)

      let ghcOpts = sharedGhcOptions ++ ["-Werror"]
          -- One compile job per fixture (ghc --make handles inter-module deps)
          jobs =
            [ ( "fixture-" ++ sanitizeLibName name
              , fixtureDir
              , mods
              , if optcWerror then ["-optc-Werror"] else []
              )
            | (name, fixtureDir, mods, optcWerror) <- casesWithModules
            , not (null mods)
            ]

      -- Compile all fixtures in parallel
      buildResults <-
        runDirectGhcBuild tmpDir envFile ghcOpts includeDirs mHaddockDir jobs

      -- Include empty fixtures as vacuously passing
      let emptyResults = Map.fromList
            [ ("fixture-" ++ sanitizeLibName name, Right ())
            | (name, _, mods, _) <- casesWithModules
            , null mods
            ]

      return $ emptyResults <> buildResults

{-------------------------------------------------------------------------------
  Module detection
-------------------------------------------------------------------------------}

-- | Detect Haskell modules in a fixture directory
--
-- Recursively finds all @.hs@ files and converts paths to module names:
-- @Example.hs@ -> @Example@, @Example\/Safe.hs@ -> @Example.Safe@.
--
detectModules :: FilePath -> IO [String]
detectModules fixtureDir = do
    hsFiles <- findHaskellFiles fixtureDir
    return $ sort [ toModuleName (makeRelative fixtureDir f) | f <- hsFiles ]
  where
    toModuleName :: FilePath -> String
    toModuleName = map sepToDot . dropExtension
      where
        sepToDot c | c == pathSeparator = '.'
        sepToDot c = c

-- | Recursively find all @.hs@ files in a directory
findHaskellFiles :: FilePath -> IO [FilePath]
findHaskellFiles dir = do
    entries <- listDirectory dir
    concat <$> mapM go entries
  where
    go :: FilePath -> IO [FilePath]
    go entry = do
        let path = dir </> entry
        isDir <- doesDirectoryExist path
        if isDir
          then findHaskellFiles path
          else return [ path | takeExtension entry == ".hs" ]
