{-# LANGUAGE RecordWildCards #-}

-- | Library-mode execution for @hs-bindgen preprocess --library@.
--
-- Walks the include graph, assigns each sub-header its own Haskell module,
-- and runs the binding generator once per module in dependency order.
-- Binding specs are chained between steps so cross-module type references
-- resolve.
--
-- Called from "HsBindgen.Cli.Preprocess" when @--library@ is present.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.PreprocessLibrary qualified as PreprocessLibrary
module HsBindgen.Cli.PreprocessLibrary (
    Opts(..)
  , parseOpts
  , exec
  ) where

import Control.Monad (foldM_)
import Data.Text qualified as Text
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)

import Clang.Paths

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice)
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec (BindingSpecConfig (..))
import HsBindgen.Config
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro
import HsBindgen.PreprocessLibrary.Naming (LibraryHeaderResult (..),
                                           deriveModuleName, detectCollisions,
                                           filterByLibraryRoot, formatCollision,
                                           moduleToPath)
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Library-mode options
--
data Opts = Opts {
      libraryRoots      :: [FilePath]
    , exceptLibraryRoot :: [Regex]
    , dryRun            :: Bool
    , listModules       :: Bool
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> many parseLibraryRoot
      <*> many parseExceptLibrary
      <*> parseDryRun
      <*> parseListModules

parseLibraryRoot :: Parser FilePath
parseLibraryRoot = strOption $ mconcat [
      long "library"
    , metavar "DIR"
    , help $ concat [
          "Activates library mode. "
        , "Directory containing library headers (module-generation scope). "
        , "Only headers whose normalised path is under a library directory "
        , "get their own Haskell module. Also determines the base path "
        , "for deriving module names. "
        , "Repeatable. "
        , "This is NOT the clang search path (-I)."
        ]
    ]

parseExceptLibrary :: Parser Regex
parseExceptLibrary = strOption $ mconcat [
      long "except-library"
    , metavar "PCRE"
    , help $ concat [
          "Exclude headers matching PCRE from module generation, "
        , "even if they fall under a --library directory. "
        , "This is a module-generation scope filter, NOT a selection predicate: "
        , "types from excluded headers remain available via program slicing."
        ]
    ]

parseDryRun :: Parser Bool
parseDryRun = switch $ mconcat [
      long "dry-run"
    , help "Show the processing plan without generating any files (library mode)"
    ]

parseListModules :: Parser Bool
parseListModules = switch $ mconcat [
      long "list-modules"
    , help "Print generated module names one per line (library mode)"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Run the library-mode pipeline: discover headers via the include graph,
-- filter by library directories, check for collisions, then generate one module
-- per header in dependency order.
exec ::
     GlobalOpts
  -> Config
  -> UniqueId
  -> BaseModuleName
  -> QualifiedStyle
  -> OutputOptions
  -> FilePath
  -> DirPolicy
  -> FilePolicy
  -> [C.UncheckedRootDirective]
  -> Opts
  -> IO ()
exec global config uniqueId baseModuleName qualifiedStyle outputOptions
    hsOutputDir dirPolicy filePolicy inputs opts = do

    -- Run hsBindgen once with a dummy config to obtain the include graph.
    let graphConfig = toBindgenConfig
          config
          (UniqueId "preprocess-library-graph")
          (BaseModuleName "unused")
          (def :: ByCategory Choice)

    includeGraph <- hsBindgen
      global.unsafe
      global.safe
      graphConfig
      inputs
      getIncludeGraph

    roots <- mapM canonicalizePath opts.libraryRoots

    -- Filter headers to those under a library directory, derive module names.
    let allHeaders = IncludeGraph.toSortedList includeGraph
        filtered   = filterByLibraryRoot roots opts.exceptLibraryRoot allHeaders
        modules    = [ (h, deriveModuleName roots baseModuleName h)
                     | h <- filtered.included
                     ]

    let checkCategories = case outputOptions.mode of
          FilePerModule -> True
          SingleFile{}  -> False

    case detectCollisions checkCategories modules of
      collisions@(_ : _) -> do
        putStrLn "Error: module name collisions detected"
        putStrLn ""
        mapM_ (putStrLn . formatCollision) collisions
        exitWith (ExitFailure 4)
      [] -> pure ()

    when opts.listModules $ do
      mapM_ (\(_, m) -> putStrLn (Text.unpack m.text)) modules
      exitSuccess

    when opts.dryRun $ do
      printPlan filtered modules
      exitSuccess

    let step = StepArgs {..}

    eErr <- withTracer global.unsafe $ \tracer -> do
      let ppTracer = contramap TracePreprocessLibrary tracer
      executePlan global step ppTracer roots (map fst modules)

    case eErr of
      Right () -> pure ()
      Left err -> do
        print $ prettyForTrace err
        exitWith (ExitFailure 3)

printPlan :: LibraryHeaderResult -> [(RealPath, BaseModuleName)] -> IO ()
printPlan filtered modules = do
    putStrLn $ concat [
        show (length modules), " modules to generate"
      , case length filtered.excluded of
          0 -> ""
          n -> " (" ++ show n ++ " excluded by --except-library)"
      ]
    putStrLn ""
    mapM_ (\(hdr, m) ->
      putStrLn $ "  " ++ getRealPath hdr ++ " -> " ++ Text.unpack m.text
      ) modules

-- | Values that are constant across all steps; bundled to avoid threading
-- nine arguments through foldM_.
data StepArgs = StepArgs {
      config         :: Config
    , uniqueId       :: UniqueId
    , baseModuleName :: BaseModuleName
    , qualifiedStyle :: QualifiedStyle
    , outputOptions  :: OutputOptions
    , hsOutputDir    :: FilePath
    , dirPolicy      :: DirPolicy
    , filePolicy     :: FilePolicy
    , inputs         :: [C.UncheckedRootDirective]
    }
  deriving (Generic)

-- | Execute plan
--
-- Each step runs the full hsBindgen pipeline with a per-header selection
-- predicate and program slicing enabled, chaining binding specs from earlier
-- steps as external specs so cross-module type references resolve.
--
executePlan ::
     GlobalOpts
  -> StepArgs
  -> Tracer PreprocessLibraryMsg
  -> [FilePath]
  -> [RealPath]
  -> IO ()
executePlan global step tracer roots headers =
    withSystemTempDirectory "hs-bindgen-library" $ \tempDir ->
      foldM_ (executeStep global step tracer roots tempDir) [] headers

executeStep ::
     GlobalOpts
  -> StepArgs
  -> Tracer PreprocessLibraryMsg
  -> [FilePath]
  -> FilePath
  -> [FilePath]
  -> RealPath
  -> IO [FilePath]
executeStep global step tracer roots tempDir accSpecs hdr = do
    let modName = deriveModuleName roots step.baseModuleName hdr
        bsPath  = tempDir </> moduleToPath modName <.> "yaml"

        -- Narrow to declarations from this header, pull transitive
        -- dependencies via program slicing and feed in the accumulated specs.
        stepConfig = step.config {
            selectionPredicate =
              BAnd (selectionFor hdr) step.config.selectionPredicate
          , programSlicing     = EnableProgramSlicing
          , bindingSpec        = step.config.bindingSpec {
                extBindingSpecs =
                  step.config.bindingSpec.extBindingSpecs ++ accSpecs
              }
          }

        bindgenConfig = toBindgenConfig
          stepConfig
          step.uniqueId
          modName
          (buildCategoryChoice step.outputOptions)

        mrc :: ModuleRenderConfig
        mrc = ModuleRenderConfig {
            qualifiedStyle = step.qualifiedStyle
          }

        artefact :: Artefact CExpr ()
        artefact = do
          case step.outputOptions of
            OutputOptions (SingleFile _) ->
              writeBindingsSingle
                mrc step.filePolicy step.dirPolicy step.hsOutputDir
            _ ->
              writeBindingsMultiple
                mrc step.filePolicy step.dirPolicy step.hsOutputDir
          writeBindingSpec step.filePolicy step.dirPolicy bsPath

    traceWith tracer $ withCallStack $
      PreprocessLibraryProcessing hdr (Text.unpack modName.text)

    createDirectoryIfMissing True (takeDirectory bsPath)
    hsBindgen global.unsafe global.safe bindgenConfig step.inputs artefact

    pure (accSpecs ++ [bsPath])

-- | Selection predicates
--
-- For each step, we build a predicate matching only declarations from the
-- target header, using PCRE \Q..\E quoting so special characters in paths
-- need no escaping.
--
selectionFor :: RealPath -> Boolean SelectionPredicate
selectionFor path =
    BIf (SelectHeader (HeaderPathMatches (exactMatch path)))
  where
    exactMatch :: RealPath -> Regex
    exactMatch (RealPath p) =
        fromString $ "^\\Q" ++ Text.unpack p ++ "\\E$"
