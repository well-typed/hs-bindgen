{-# LANGUAGE RecordWildCards #-}

-- | Library-mode execution for @hs-bindgen preprocess --library@.
--
-- Walks the include graph, assigns each sub-header its own Haskell module
-- (headers that include each other share one), and runs the binding generator
-- once per module in dependency order.
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
import Data.List qualified as List
import Data.Set qualified as Set
import Data.Text qualified as Text
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing,
                         doesDirectoryExist)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)

import Clang.Paths

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice)
import HsBindgen.Artefact (FrontendPass (..))
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
                                           LibraryUnit (..), declaringHeaders,
                                           detectCollisions,
                                           filterByLibraryRoot, formatCollision,
                                           mkLibraryUnit, moduleToPath)
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

-- | Library-mode options
--
data Opts = Opts {
      libraryRoots      :: [FilePath]
    , exceptLibraryRoot :: [Regex]
    , dryRun            :: Bool
    , listModules       :: Bool
    , genBindingSpecDir :: Maybe FilePath
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> many parseLibraryRoot
      <*> many parseExceptLibrary
      <*> parseDryRun
      <*> parseListModules
      <*> optional parseGenBindingSpecDir

parseLibraryRoot :: Parser FilePath
parseLibraryRoot = strOption $ mconcat [
      long "library"
    , metavar "DIR"
    , help $ concat [
          "Activates library mode. "
        , "Directory containing library headers (module-generation scope). "
        , "Only headers whose normalised path is under a library directory "
        , "get a Haskell module; headers that include each other share one. "
        , "Also determines the base path "
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

parseGenBindingSpecDir :: Parser FilePath
parseGenBindingSpecDir = strOption $ mconcat [
      long "gen-binding-spec-dir"
    , metavar "DIR"
    , help $ concat [
          "Directory to write per-module binding specifications to "
        , "(library mode). Without it they go to a temporary directory "
        , "that is removed after the run."
        ]
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | Run the library-mode pipeline: discover headers via the include graph,
-- filter by library directories, check for collisions, then generate one module
-- per unit (a header, or the headers of one include cycle) in dependency
-- order.
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

    -- Run hsBindgen once with a dummy config to obtain the include graph and
    -- the headers that declare something. Both come from the parse pass.
    let graphConfig = toBindgenConfig
          config
          (UniqueId "preprocess-library-graph")
          (BaseModuleName "unused")
          (def :: ByCategory Choice)

    (includeGraph, declaring) <- hsBindgen
      global.unsafe
      global.safe
      graphConfig
      inputs
      ((,) <$> getIncludeGraph
           <*> (declaringHeaders <$> FrontendPassA ParsePass))

    roots <- mapM canonicalizePath opts.libraryRoots

    -- Keep the headers under a library directory that declare something, then
    -- name one module per include-cycle group.
    let components = IncludeGraph.toSortedComponents includeGraph
        filtered   = filterByLibraryRoot roots opts.exceptLibraryRoot
                       (`Set.member` declaring) components
        units      = map (mkLibraryUnit roots baseModuleName) filtered.included

    let checkCategories = case outputOptions.mode of
          FilePerModule -> True
          SingleFile{}  -> False

    case detectCollisions checkCategories units of
      collisions@(_ : _) -> do
        putStrLn "Error: module name collisions detected"
        putStrLn ""
        mapM_ (putStrLn . formatCollision) collisions
        exitWith (ExitFailure 4)
      [] -> pure ()

    -- The module list is meant for a @.cabal@ file's @exposed-modules@.
    -- Generating the @.cabal@ file itself is tracked in
    -- <https://github.com/well-typed/hs-bindgen/issues/2103>.
    when opts.listModules $ do
      mapM_ (\unit -> putStrLn (Text.unpack unit.moduleName.text)) units
      exitSuccess

    when opts.dryRun $ do
      printPlan filtered units
      exitSuccess

    -- Same rule as for --hs-output-dir: the directory itself needs
    -- --create-output-dirs, module subdirectories are created as needed.
    forM_ opts.genBindingSpecDir $ \dir -> do
      exists <- doesDirectoryExist dir
      unless (exists || dirPolicy == CreateOutputDirs) $ do
        putStrLn $ "Error: binding spec directory does not exist: " ++ dir
        exitWith (ExitFailure 4)

    let step = StepArgs {..}

    eErr <- withTracer global.unsafe $ \tracer -> do
      let ppTracer = contramap TracePreprocessLibrary tracer
      executePlan global step ppTracer opts.genBindingSpecDir units

    case eErr of
      Right () -> pure ()
      Left err -> do
        print $ prettyForTrace err
        exitWith (ExitFailure 3)

-- | Print one line per header; the headers of an include cycle share a module.
printPlan :: LibraryHeaderResult -> [LibraryUnit] -> IO ()
printPlan filtered units = do
    putStrLn $ concat [
        show (length units), " modules to generate"
      , case notes of
          [] -> ""
          _  -> " (" ++ List.intercalate "; " notes ++ ")"
      ]
    putStrLn ""
    forM_ units $ \unit -> forM_ unit.headers $ \hdr ->
      putStrLn $ concat [
          "  ", getRealPath hdr, " -> ", Text.unpack unit.moduleName.text
        ]
  where
    cycles :: Int
    cycles = length [ () | unit <- units, length unit.headers > 1 ]

    notes :: [String]
    notes = concat [
        [ concat [
              show (sum [ length unit.headers | unit <- units ]), " headers, "
            , show cycles, " include cycle", if cycles == 1 then "" else "s"
            ]
        | cycles > 0
        ]
      , [ show n ++ " excluded by --except-library"
        | let n = length filtered.excluded
        , n > 0
        ]
      , [ show n ++ (if n == 1 then " header declares" else " headers declare")
            ++ " nothing"
        | let n = length filtered.withoutDecls
        , n > 0
        ]
      ]

-- | Values that are constant across all steps; bundled to avoid threading
-- eight arguments through foldM_.
data StepArgs = StepArgs {
      config         :: Config
    , uniqueId       :: UniqueId
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
-- The specs are written to the @--gen-binding-spec-dir@ directory when one is
-- given; otherwise they live in a temporary directory removed after the run.
--
executePlan ::
     GlobalOpts
  -> StepArgs
  -> Tracer PreprocessLibraryMsg
  -> Maybe FilePath
  -> [LibraryUnit]
  -> IO ()
executePlan global step tracer mSpecDir units =
    case mSpecDir of
      Just specDir -> run specDir
      Nothing      -> withSystemTempDirectory "hs-bindgen-library" run
  where
    run :: FilePath -> IO ()
    run specDir =
      foldM_ (executeStep global step tracer specDir) [] units

executeStep ::
     GlobalOpts
  -> StepArgs
  -> Tracer PreprocessLibraryMsg
  -> FilePath
  -> [FilePath]
  -> LibraryUnit
  -> IO [FilePath]
executeStep global step tracer specDir accSpecs unit = do
    let modName = unit.moduleName
        bsPath  = specDir </> moduleToPath modName <.> "yaml"

        -- Narrow to declarations from this unit's headers, pull transitive
        -- dependencies via program slicing and feed in the accumulated specs.
        stepConfig = step.config {
            selectionPredicate =
              BAnd (unitSelection unit) step.config.selectionPredicate
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
      PreprocessLibraryProcessing unit.headers (Text.unpack modName.text)

    createDirectoryIfMissing True (takeDirectory bsPath)
    hsBindgen global.unsafe global.safe bindgenConfig step.inputs artefact

    pure (accSpecs ++ [bsPath])

-- | Selection predicates
--
-- For each step, we build a predicate matching only declarations from the
-- unit's headers, using PCRE \Q..\E quoting so special characters in paths
-- need no escaping.
--
unitSelection :: LibraryUnit -> Boolean SelectionPredicate
unitSelection unit = foldr1 BOr (fmap selectionFor unit.headers)

selectionFor :: RealPath -> Boolean SelectionPredicate
selectionFor path =
    BIf (SelectHeader (HeaderPathMatches (exactMatch path)))
  where
    exactMatch :: RealPath -> Regex
    exactMatch (RealPath p) =
        fromString $ "^\\Q" ++ Text.unpack p ++ "\\E$"
