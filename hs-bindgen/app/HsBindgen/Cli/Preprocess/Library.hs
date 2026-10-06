-- | Library mode of @hs-bindgen-cli preprocess@, enabled with @--library@
--
-- A planning run of the frontend over the library learns which declarations
-- are generated and which of them use which, and assigns each library header
-- its own Haskell module (headers whose declarations use each other share
-- one). The binding generator then runs once per module, frontend included,
-- after the modules it uses. Binding specs are chained between steps so
-- cross-module type references resolve.
--
-- Called from "HsBindgen.Cli.Preprocess" when @--library@ is present.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Preprocess.Library qualified as Library
module HsBindgen.Cli.Preprocess.Library (
    -- * Options
    Opts(..)
  , parseOpts
    -- * Execution
  , RunOpts(..)
  , exec
  ) where

import Control.Monad (foldM_)
import Options.Applicative
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (addTrailingPathSeparator, isPathSeparator,
                        pathSeparators, replaceExtension, takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)

import Clang.Paths

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputOptions (..), buildCategoryChoice,
                             writeBindingsWith)
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec (BindingSpecConfig (..))
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.Config.Prelims (fromBaseModuleName)
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Language.Haskell qualified as Hs
import HsBindgen.LibraryMode.Plan (LibraryPlan (..), LibraryUnit (..),
                                   planLibrary)
import HsBindgen.Macro
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Library-mode options
data Opts = Opts {
      libraryRoots :: [FilePath]
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> many parseLibraryRoot

parseLibraryRoot :: Parser FilePath
parseLibraryRoot = strOption $ mconcat [
      long "library"
    , metavar "DIR"
    , help $ concat [
          "Activates library mode. "
        , "Directory containing library headers (module-generation scope). "
        , "A header gets a Haskell module when the HEADER arguments include "
        , "it, directly or through other headers, and its normalised path is "
        , "under a library directory; headers whose declarations use each "
        , "other share one. The directory itself is not scanned. "
        , "Also determines the base path for deriving module names. "
        , "Repeatable. "
        , "This is not the clang search path (-I)."
        ]
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

-- | The options of @preprocess@ that every run of library mode shares: the
-- planning run and the run for each module
data RunOpts = RunOpts {
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

-- | Run the library-mode pipeline: run the frontend over the library to plan
-- one module per unit (a header, or the headers of one declaration loop), then
-- generate the modules, one full run each, after the units they use.
exec :: GlobalOpts -> RunOpts -> Opts -> IO ()
exec global runOpts opts = do
    -- Canonical paths, so that they compare with the 'RealPath' of a header
    roots <- mapM canonicalizePath opts.libraryRoots

    -- Planning run of the frontend, selecting what the steps select between
    -- them: the library headers, the user's predicate, and program slicing.
    -- This tells us which declarations are generated, where they are, and
    -- which of them use which. No bindings are written.
    let planConfig = toBindgenConfig
          (narrowTo (libraryScope roots) runOpts.config)
          (UniqueId "library-mode-plan")
          (BaseModuleName "unused")
          (def :: ByCategory Choice)

    (includeGraph, decls, useDeclGraph) <- hsBindgen
      global.unsafe
      global.safe
      planConfig
      runOpts.inputs
      ((,,) <$> getIncludeGraph <*> getReifiedC <*> getUseDeclGraph)

    -- Plan: one unit per module, each after the units whose declarations it
    -- uses
    let plan = planLibrary roots runOpts.baseModuleName
                 includeGraph useDeclGraph decls

    -- Generate: one run per unit, in order
    eErr <- withTracer global.unsafe $ \tracer -> do
      let stepTracer = contramap TraceLibraryMode tracer
      executePlan global runOpts stepTracer plan.units

    case eErr of
      Right () -> pure ()
      Left err -> do
        print $ prettyForTrace err
        -- The same exit code as 'hsBindgen' uses for an error trace: the run
        -- got to its end, but an error has occurred.
        exitWith (ExitFailure 4)

-- | The module a unit keeps its types in, which is also the module its
-- binding specification is for
typeModule :: BaseModuleName -> Hs.ModuleName
typeModule name = fromBaseModuleName name (Just CType)

{-------------------------------------------------------------------------------
  Generation
-------------------------------------------------------------------------------}

-- | Execute plan
--
-- Each step runs the full hsBindgen pipeline with a per-unit selection
-- predicate and program slicing enabled, chaining binding specs from earlier
-- steps as external specs so cross-module type references resolve.
--
-- The specs live in a temporary directory that is removed after the run.
executePlan ::
     GlobalOpts
  -> RunOpts
  -> Tracer LibraryModeMsg
  -> [LibraryUnit]
  -> IO ()
executePlan global runOpts tracer units =
    withSystemTempDirectory "hs-bindgen-library" $ \specDir ->
      foldM_ (executeStep global runOpts tracer specDir) [] units

-- | Generate the module of one unit and its binding specification
--
-- Takes the binding specifications of the steps before it, and returns them
-- with its own added.
executeStep ::
     GlobalOpts
  -> RunOpts
  -> Tracer LibraryModeMsg
  -> FilePath
     -- ^ Directory for the binding specifications
  -> [FilePath]
  -> LibraryUnit
  -> IO [FilePath]
executeStep global runOpts tracer specDir accSpecs unit = do
    traceWith tracer $ withCallStack $
      LibraryModeProcessing unit.headers unit.moduleName

    createDirectoryIfMissing True (takeDirectory specPath)
    hsBindgen global.unsafe global.safe bindgenConfig runOpts.inputs artefact

    pure (accSpecs ++ [specPath])
  where
    specPath :: FilePath
    specPath = specDir </>
      replaceExtension (Hs.moduleNamePath (typeModule unit.moduleName)) "yaml"

    -- Narrow to declarations from this unit's headers, pull transitive
    -- dependencies via program slicing and feed in the accumulated specs.
    stepConfig :: Config
    stepConfig = (narrowTo (unitSelection unit) runOpts.config) {
          bindingSpec = runOpts.config.bindingSpec {
              extBindingSpecs =
                runOpts.config.bindingSpec.extBindingSpecs ++ accSpecs
            }
        }

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          stepConfig
          runOpts.uniqueId
          unit.moduleName
          (buildCategoryChoice runOpts.outputOptions)

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
          qualifiedStyle = runOpts.qualifiedStyle
        }

    artefact :: Artefact CExpr ()
    artefact = do
        writeBindingsWith
          runOpts.outputOptions
          mrc
          runOpts.filePolicy
          runOpts.dirPolicy
          runOpts.hsOutputDir
        writeBindingSpec runOpts.filePolicy runOpts.dirPolicy specPath

{-------------------------------------------------------------------------------
  Selection
-------------------------------------------------------------------------------}

-- | The declarations of one unit's headers
--
-- The paths are matched literally (see 'quoteRegex').
unitSelection :: LibraryUnit -> Boolean SelectionPredicate
unitSelection unit = foldr1 BOr (fmap selectionFor unit.headers)

selectionFor :: RealPath -> Boolean SelectionPredicate
selectionFor path =
    headerMatches $ fromString ("^" ++ quoteRegex (getRealPath path) ++ "$")

-- | Declarations from headers that may get a module
--
-- The scope of 'planLibrary' as a predicate: under a library directory. The
-- planning run selects this together with the user's predicate, which is what
-- the steps select between them.
libraryScope :: [FilePath] -> Boolean SelectionPredicate
libraryScope roots = mergeBooleans [] (map underRoot roots)
  where
    underRoot :: FilePath -> Boolean SelectionPredicate
    underRoot root = headerMatches $
        fromString ("^" ++ pathRegex (addTrailingPathSeparator root))

-- | A regular expression that matches a path of this platform literally, with
-- any directory separator where the path has one
--
-- A library directory is written the way @canonicalizePath@ writes it, and the
-- 'RealPath' of a header the way libclang does. On Windows the two can differ:
-- the directory has backslashes, and libclang may answer with slashes.
--
-- > C:\lib\    has to match    C:/lib/a.h
pathRegex :: FilePath -> String
pathRegex path = case break isPathSeparator path of
    (name, [])       -> quoteRegex name
    (name, _ : rest) -> quoteRegex name ++ anySeparator ++ pathRegex rest
  where
    anySeparator :: String
    anySeparator = "[" ++ concatMap (\c -> ['\\', c]) pathSeparators ++ "]"

-- | Select what both the given predicate and the user's select, plus what
-- that uses
--
-- The planning run and the steps narrow the configuration the same way, so
-- the plan sees what the steps generate between them.
narrowTo :: Boolean SelectionPredicate -> Config -> Config
narrowTo predicate config = config {
      selectionPredicate = BAnd predicate config.selectionPredicate
    , programSlicing     = EnableProgramSlicing
    }

headerMatches :: Regex -> Boolean SelectionPredicate
headerMatches = BIf . SelectHeader . HeaderPathMatches
