-- | @hs-bindgen-cli preprocess-library@ command
--
-- Generate Haskell modules for a C library whose public API spans multiple
-- headers. Where @preprocess@ targets a single header with a single selection
-- predicate, @preprocess-library@ walks the include graph of the given
-- root header(s), assigns each sub-header its own Haskell module, and runs
-- @hsBindgen@ once per module in dependency order.
--
-- == Public and internal headers
--
-- C libraries expose their public API through bracket-includable headers
-- (@\<widget.h\>@), resolved via the search include paths. Internally,
-- sub-headers reference each other with quote includes
-- (@\"widget\/core.h\"@, @\"widget\/util.h\"@), which resolve relative to
-- the including file's directory first. A library's consumers use bracket
-- includes; quote includes are for in-project references between the
-- library's own files.
--
-- For bindings to an external library, the header must be accessed by a bracket
-- include. This is because the generated C code is compiled from a build
-- directory controlled by Cabal or GHC, not from the library's source
-- tree, so a quote include could resolve against the wrong directory.
-- hs-bindgen never invents or rewrites include paths; what the user supplies
-- is what goes into the generated code.
--
-- == Approach
--
-- The header on the command line determines both which declarations get
-- bindings and what appears in the generated @#include@. To split a
-- library across multiple Haskell modules, @preprocess-library@ separates
-- them using /selection predicates/.
--
-- Every iteration passes the same user-supplied root header(s) as positional
-- arguments, preserving the correct bracket @#include@. A per-step
-- @SelectHeader (HeaderPathMatches regex)@ predicate controls which
-- declarations get bindings, narrowing each run to one sub-header.
--
-- Binding specs from earlier iterations are fed as external specs to later
-- ones, so cross-module type references resolve correctly. Combined with
-- @EnableProgramSlicing@, types from excluded headers are pulled in as
-- transitive dependencies when needed, and shared across modules via binding
-- spec chaining.
--
-- == Module naming
--
-- Each header's relative path under the library root is split into
-- components, each component is capitalized, and the result is joined with
-- dots under the @--module@ prefix. For example, @--module Widget@ with a
-- header at @widget\/core.h@ relative to the library root produces
-- @Widget.Core@.
--
-- == Example
--
-- Given a library with:
--
-- @
-- \<widget.h\>            -- bracket-includable, #include "widget/core.h" etc.
--   "widget\/core.h"
--   "widget\/util.h"
--   "widget\/internal.h"  -- private, should not get its own module
-- @
--
-- @
-- hs-bindgen preprocess-library \\
--   -I\/usr\/include \\
--   --module Widget \\
--   --hs-output-dir gen \\
--   --library-root \/usr\/include\/widget \\
--   --exclude-header 'internal' \\
--   --create-output-dirs \\
--   --overwrite-files \\
--   widget.h
-- @
--
-- This produces modules @Widget.Core@, @Widget.Util@, and @Widget@ (the root).
-- No module is generated for @widget\/internal.h@, but any types it defines
-- that are needed by other modules are pulled in via program slicing and
-- shared through binding spec chaining.
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.PreprocessLibrary qualified as PreprocessLibrary
module HsBindgen.Cli.PreprocessLibrary (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad (foldM_)
import Data.Text qualified as Text
import Options.Applicative hiding (info)
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (<.>), (</>))
import System.IO.Temp (withSystemTempDirectory)

import Clang.Paths

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.BindingSpec (BindingSpecConfig (..))
import HsBindgen.Config
import HsBindgen.Config.ClangArgs (ClangArgsConfig (..))
import HsBindgen.Frontend.Pass.Select.IsPass (ProgramSlicing (..))
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro
import HsBindgen.PreprocessLibrary.Plan (Plan (..), PlanStep (..))
import HsBindgen.PreprocessLibrary.Plan qualified as Plan
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ concat [
    "Generate Haskell modules for a C library with multiple headers. "
  , "Walks the include graph, assigns each sub-header its own module, "
  , "and runs hs-bindgen once per module in dependency order. "
  , "Each iteration passes the root header for the generated #include "
  , "and uses a selection predicate to target one sub-header's declarations. "
  , "Module names are derived from header paths: --module Widget with "
  , "widget/core.h under the library root produces Widget.Core."
  ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for the @preprocess-library@ subcommand.
data Opts = Opts {
      -- | Clang arguments, binding spec config, field naming, etc.
      config       :: Config

      -- | Unique identifier to disambiguate generated C symbols.
    , uniqueId     :: UniqueId

      -- | Module name prefix. Each sub-header's module name is derived by
      -- appending its relative path components to this prefix.
      -- For example, with @--module Widget@ a header at @widget\/core.h@
      -- relative to the library root becomes @Widget.Core@.
    , baseModule   :: BaseModuleName

      -- | Import style for generated modules.
    , qualStyle    :: QualifiedStyle

      -- | Controls single-file vs. file-per-module output.
    , outputOpts   :: OutputOptions

      -- | Directory where generated Haskell modules are written.
    , hsOutputDir  :: FilePath

      -- | Whether to create output directories that do not exist.
    , dirPolicy    :: DirPolicy

      -- | Whether to overwrite existing output files.
    , filePolicy   :: FilePolicy

      -- | Directories that define the library boundary. Only headers whose
      -- canonical path falls under one of these roots will produce a Haskell
      -- module. Defaults to the @-I@ include directories when omitted.
      --
      -- The library roots should be at most as wide as (and ideally a subset
      -- of) the @-I@ directories, because headers outside the include search
      -- path cannot be resolved by clang. If a library root is wider than
      -- every @-I@ directory, a warning is emitted.
    , libraryRoots :: [FilePath]

      -- | PCRE patterns for headers that should not get their own module.
      -- Excluded headers are simply skipped: no module is generated for them.
      -- Types from excluded headers are still available to other modules via
      -- program slicing (which pulls in transitive type dependencies) and
      -- shared across modules via binding spec chaining. Functions from
      -- excluded headers get no bindings.
    , excludeHdrs  :: [Regex]

      -- | When set, print the processing plan and exit without generating
      -- any files.
    , dryRun       :: Bool

      -- | When set, print generated module names one per line, suitable for
      -- pasting into a @.cabal@ file.
    , listModules  :: Bool

      -- | The root directives (positional HEADER arguments). These are the
      -- bracket-includable headers that clang will parse. The same set is
      -- passed to every @hsBindgen@ invocation; only the selection predicate
      -- varies between steps.
    , inputs       :: [C.UncheckedRootDirective]
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts = Opts
    <$> parseConfigLibrary
    <*> parseUniqueId
    <*> parseBaseModuleName
    <*> parseQualifiedStyle
    <*> parseOutputOptions FilePerModule
    <*> parseHsOutputDir
    <*> parseDirPolicy
    <*> parseFilePolicy
    <*> many parseLibraryRoot
    <*> many parseExcludeHeader
    <*> parseDryRun
    <*> parseListModules
    <*> parseInputs

-- | Parse the subset of 'Config' relevant to @preprocess-library@.
--
-- Selection predicates and program slicing are not exposed because they are
-- computed per-step internally.
parseConfigLibrary :: Parser Config
parseConfigLibrary = Config
    <$> parseClangArgsConfig
    <*> parseBindingSpec
    <*> pure (def :: Boolean SelectionPredicate)
    <*> pure DisableProgramSlicing
    <*> parseFieldNamingStrategy
    <*> parsePathStyle

parseLibraryRoot :: Parser FilePath
parseLibraryRoot = strOption $ mconcat [
      long "library-root"
    , metavar "DIR"
    , help $ concat [
          "Restrict module generation to headers under DIR. "
        , "Repeatable. Defaults to the -I directories when omitted. "
        , "Should not be wider than the -I directories"
        ]
    ]

parseExcludeHeader :: Parser Regex
parseExcludeHeader = strOption $ mconcat [
      long "exclude-header"
    , metavar "PCRE"
    , help $ concat [
          "Exclude headers whose path matches PCRE from getting their own "
        , "module. Types from excluded headers are still available via "
        , "program slicing"
        ]
    ]

parseDryRun :: Parser Bool
parseDryRun = switch $ mconcat [
      long "dry-run"
    , help "Show the processing plan without generating any files"
    ]

parseListModules :: Parser Bool
parseListModules = switch $ mconcat [
      long "list-modules"
    , help "Print generated module names one per line (for .cabal files)"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
    let graphConfig = toBindgenConfig
          opts.config
          (UniqueId "preprocess-library-graph")
          (BaseModuleName "unused")
          (def :: ByCategory Choice)

    includeGraph <- hsBindgen
      global.unsafe
      global.safe
      graphConfig
      opts.inputs
      getIncludeGraph

    let ClangArgsConfig{extraIncludeDirs = includeDirs} = opts.config.clang
    roots <- resolveLibraryRoots opts.libraryRoots includeDirs

    eErr <- withTracer global.unsafe $ \tracer -> do
      let ppTracer = contramap TracePreprocessLibrary tracer
      unless (null opts.libraryRoots) $
        warnWideRoots ppTracer roots includeDirs

      let plan = Plan.computePlan includeGraph roots opts.excludeHdrs opts.baseModule

      when opts.listModules $
        printModuleList plan

      if opts.dryRun
        then printPlan plan
        else executePlan global opts ppTracer plan

    case eErr of
      Right () -> pure ()
      Left err -> do
        print $ prettyForTrace err
        exitWith (ExitFailure 3)

{-------------------------------------------------------------------------------
  Library root resolution
-------------------------------------------------------------------------------}

-- | Determine the effective library roots.
--
-- When the user does not supply @--library-root@, the @-I@ include directories
-- are used as a fallback.
--
-- We use 'canonicalizePath' (which resolves both @..@ segments and symlinks)
-- rather than 'System.Directory.makeAbsolute' (which resolves neither).
-- The include graph stores canonical, and the plan uses prefix matching to
-- decide which headers fall under a library root. Both sides of that
-- comparison must be in the same canonical form.
--
-- Note that @-I@ flags themselves do not need canonicalization: they are
-- passed straight to clang and never compared against include graph paths.
--
resolveLibraryRoots :: [FilePath] -> [FilePath] -> IO [FilePath]
resolveLibraryRoots explicitRoots includeDirs =
    mapM canonicalizePath $ case explicitRoots of
      [] -> includeDirs
      rs -> rs

-- | Warn when a @--library-root@ is not contained in any @-I@ directory.
--
-- A library root wider than the include search path is almost certainly a
-- mistake: headers outside the @-I@ directories cannot be resolved by clang
-- during parsing, so the extra breadth would silently match nothing.
--
warnWideRoots ::
     Tracer PreprocessLibraryMsg
  -> [FilePath]
  -> [FilePath]
  -> IO ()
warnWideRoots tracer roots includeDirs = do
    canonIncDirs <- mapM canonicalizePath includeDirs
    forM_ roots $ \root ->
      unless (any (root `Plan.isUnderDir`) canonIncDirs) $
        traceWith tracer $ withCallStack $
          PreprocessLibraryWideRoot root

{-------------------------------------------------------------------------------
  Dry-run output
-------------------------------------------------------------------------------}

printPlan :: Plan -> IO ()
printPlan plan = do
    let nHeaders  = length plan.steps + length plan.excludedByUser
        nModules  = length plan.steps
        nExcluded = length plan.excludedByUser
    putStrLn $ concat [
        "Plan: "
      , show nHeaders, " headers -> "
      , show nModules, " modules"
      , if nExcluded > 0
          then " (" ++ show nExcluded ++ " excluded)"
          else ""
      ]
    putStrLn ""

    forM_ (zip [1 :: Int ..] plan.steps) $ \(i, step) ->
      putStrLn $ concat [
          "  ", show i, ". "
        , getSourcePath step.stepHeader
        , " -> ", Text.unpack step.stepModule.text
        ]

    unless (null plan.excludedByRoot) $ do
      putStrLn ""
      putStrLn $ "Excluded by --library-root: "
        ++ show (length plan.excludedByRoot) ++ " headers"

    unless (null plan.excludedByUser) $ do
      putStrLn ""
      putStrLn "Excluded by --exclude-header:"
      forM_ plan.excludedByUser $ \excluded ->
        putStrLn $ "  " ++ getSourcePath excluded

-- | Print module names one per line for use in @.cabal@ files.
printModuleList :: Plan -> IO ()
printModuleList plan =
    forM_ plan.steps $ \step ->
      putStrLn $ Text.unpack step.stepModule.text

{-------------------------------------------------------------------------------
  Execute plan

  Each step runs the full hsBindgen pipeline with:

  - The user's original root header(s) as positional arguments.
  - A per-step selection predicate that targets only declarations from
    the step's header.
  - Program slicing enabled, so transitive type dependencies are pulled
    in even if they live in a different header.
  - All binding specs from previous steps passed as external specs, so
    cross-module type references resolve.

  The selection predicate is the key mechanism: for a step targeting
  widget/core.h, we construct

    SelectHeader (HeaderPathMatches "^\\Qwidget/core.h\\E$")

  This tells hsBindgen to generate bindings only for declarations whose
  source location is in widget/core.h. The main header (widget.h) is still
  parsed in full, we just filter which declarations make it to the backend.
-------------------------------------------------------------------------------}

executePlan ::
     GlobalOpts
  -> Opts
  -> Tracer PreprocessLibraryMsg
  -> Plan
  -> IO ()
executePlan global opts tracer plan =
    withSystemTempDirectory "hs-bindgen-library" $ \tempDir ->
      foldM_ (executeStep global opts tracer tempDir) [] plan.steps

-- | Run one step of the plan, producing a Haskell module and a binding spec.
--
-- Returns the updated list of binding spec paths (previous specs plus the
-- one generated by this step).
executeStep ::
     GlobalOpts
  -> Opts
  -> Tracer PreprocessLibraryMsg
  -> FilePath
  -- ^ Temporary directory for intermediate binding specs
  -> [FilePath]
  -- ^ Binding specs accumulated from previous steps
  -> PlanStep
  -> IO [FilePath]
executeStep global opts tracer tempDir accSpecs step = do
    let bsPath     = tempDir </> Plan.moduleToPath step.stepModule <.> "yaml"

        stepConfig = opts.config {
            selectionPredicate = selectionFor step.stepHeader
          , programSlicing     = EnableProgramSlicing
          , bindingSpec        = opts.config.bindingSpec {
                extBindingSpecs =
                  opts.config.bindingSpec.extBindingSpecs ++ accSpecs
              }
          }

        bindgenConfig = toBindgenConfig
          stepConfig
          opts.uniqueId
          step.stepModule
          (buildCategoryChoice opts.outputOpts)

        mrc :: ModuleRenderConfig
        mrc = ModuleRenderConfig { qualifiedStyle = opts.qualStyle }

        artefact :: Artefact CExpr ()
        artefact = do
          case opts.outputOpts of
            OutputOptions (SingleFile _) ->
              writeBindingsSingle
                mrc
                opts.filePolicy
                opts.dirPolicy
                opts.hsOutputDir
            _ ->
              writeBindingsMultiple
                mrc
                opts.filePolicy
                opts.dirPolicy
                opts.hsOutputDir
          writeBindingSpec opts.filePolicy opts.dirPolicy bsPath

    traceWith tracer $ withCallStack $
      PreprocessLibraryProcessing step.stepHeader (Text.unpack step.stepModule.text)

    createDirectoryIfMissing True (takeDirectory bsPath)
    hsBindgen global.unsafe global.safe bindgenConfig opts.inputs artefact

    pure (accSpecs ++ [bsPath])

{-------------------------------------------------------------------------------
  Selection predicates

  A selection predicate controls which C declarations get Haskell bindings
  in a given hsBindgen run. For preprocess-library, we construct a
  Boolean SelectionPredicate that matches declarations by the canonical
  path of the header they were declared in.

  The \\Q...\\E PCRE syntax quotes the path literally, so special
  characters (dots, slashes) need no escaping.

  Combined with EnableProgramSlicing, this ensures that:
  - Only declarations physically in the target header get bindings.
  - Types they depend on (even from other headers) are pulled in as
    needed for the generated code to compile.
-------------------------------------------------------------------------------}

-- | Build a selection predicate that accepts only declarations from the
-- given header path.
selectionFor :: SourcePath -> Boolean SelectionPredicate
selectionFor path =
    BIf (SelectHeader (HeaderPathMatches (exactMatch path)))
  where
    exactMatch :: SourcePath -> Regex
    exactMatch (SourcePath p) =
        fromString $ "^\\Q" ++ Text.unpack p ++ "\\E$"

