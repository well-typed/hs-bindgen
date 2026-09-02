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
-- library across multiple Haskell modules, @preprocess-library@ assigns
-- each header its own step.
--
-- Every iteration passes the same user-supplied root header(s) as positional
-- arguments, preserving the correct bracket @#include@. A per-step
-- @SelectHeader (HeaderPathMatches regex)@ predicate narrows each run to
-- declarations from one sub-header.
--
-- Binding specs from earlier iterations are fed as external specs to later
-- ones, so cross-module type references resolve correctly. Combined with
-- @EnableProgramSlicing@, types from headers outside the library root are
-- pulled in as transitive dependencies when needed, and shared across
-- modules via binding spec chaining.
--
-- == Module-generation scope vs. selection predicates
--
-- Two independent questions control the output:
--
-- 1. /Which headers get their own Haskell module?/ Answered by
--    @--library-root@ (required) and @--except-library-root@ (optional).
--    A header in the include graph gets a module if and only if its
--    canonical path falls under a library root and does not match an
--    exclusion pattern. This question is answerable upfront, before any
--    @hsBindgen@ run.
--
-- 2. /Which declarations get bindings?/ Answered by selection predicates
--    (@--select-by-header-path@, @--select-by-decl-name@, etc.). These
--    are evaluated per-step inside @hsBindgen@. Program slicing can pull
--    types from ANY header (even outside the library root) into the
--    output, so selection predicates cannot reliably determine which
--    headers produce output files.
--
-- The @-I@ directories are the clang search path only. They have no effect
-- on either question.
--
-- == Module naming
--
-- Module names are derived from each header's canonical path relative to
-- one of the @--library-root@ directories (canonicalized, so symlinks and
-- @..@ segments are resolved before matching). The file extension is dropped,
-- each path component is capitalized, and the result is joined with dots
-- under the @--module@ prefix. For example, @--module Widget@ with a
-- header at @widget\/core.h@ relative to a library root produces
-- @Widget.Widget.Core@.
--
-- == Module name collisions
--
-- Detected before generation; the subcommand exits with an error.
--
-- * /First-character case folding/: @foo.h@ and @Foo.h@ both produce
--   @Foo@ (@capitalize@ only uppercases the first character, so
--   @FOO.h@ is distinct).
-- * /Dot-slash equivalence/: @Widget.Core.h@ retains a dot after
--   extension stripping, yielding the same module as @widget\/core.h@.
-- * /Category overlap/ (@FilePerModule@ only): @foo.h@ (module @M.Foo@)
--   and @foo\/safe.h@ (module @M.Foo.Safe@) collide at @M\/Foo\/Safe.hs@
--   because @M.Foo@'s Safe category and @M.Foo.Safe@'s Types file share
--   that path.
--
-- Use @--except-library-root@ to exclude one side of a collision, or
-- adjust @--library-root@ directories to change the derived paths.
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
--   --library-root \/usr\/include\/widget \\
--   --module Widget \\
--   --hs-output-dir gen \\
--   --except-library-root 'internal' \\
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
import System.Exit (ExitCode (..), exitSuccess, exitWith)
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

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ concat [
    "Generate Haskell modules for a C library with multiple headers. "
  , "Walks the include graph and generates one module per header under "
  , "the --library-root directories, in dependency order. "
  , "Use --except-library-root PCRE to exclude specific headers from getting "
  , "their own modules. "
  , "Selection predicates (--select-*) independently control which "
  , "declarations get bindings within each module."
  ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

-- | Options for the @preprocess-library@ subcommand.
data Opts = Opts {
      -- | Clang arguments, binding spec config, field naming, etc.
      --
      -- Selection predicates in the config control which /declarations/
      -- get bindings within each module. They are independent of
      -- @--library-root@ and @--except-library-root@, which control
      -- which /headers/ get their own module.
      config       :: Config

      -- | Unique identifier to disambiguate generated C symbols.
    , uniqueId     :: UniqueId

      -- | Module name prefix. Each sub-header's module name is derived by
      -- appending its relative path components to this prefix.
      -- For example, with @--module Widget@ a header at @widget\/core.h@
      -- relative to a @--library-root@ directory becomes @Widget.Widget.Core@.
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

      -- | Directories containing library headers. A header in the include
      -- graph gets its own module if and only if its canonical path falls
      -- under one of these roots. Also used as the base path for deriving
      -- module names.
      --
      -- This is NOT the clang search path (@-I@). The @-I@ directories
      -- tell clang where to find headers during parsing. Library roots
      -- tell @preprocess-library@ which of those headers belong to the
      -- library and should each get their own Haskell module.
    , libraryRoots :: [FilePath]

      -- | PCRE patterns for headers to exclude from module generation,
      -- even when they fall under a @--library-root@. This is a
      -- module-generation scope filter, NOT a selection predicate.
      --
      -- Types from excluded headers remain available to other modules
      -- via program slicing and binding spec chaining.
    , exceptLibraryRoot :: [Regex]

      -- | When set, print the processing plan and exit without
      -- generating any files.
    , dryRun       :: Bool

      -- | When set, print generated module names one per line (for
      -- pasting into a @.cabal@ file) and exit.
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
    <*> some parseLibraryRoot
    <*> many parseExceptLibraryRoot
    <*> parseDryRun
    <*> parseListModules
    <*> parseInputs

-- | Parse the subset of 'Config' relevant to @preprocess-library@.
--
-- Selection predicates exposed here control which /declarations/ receive
-- Haskell bindings within each generated module. They are independent of
-- @--library-root@ and @--except-library-root@, which control which
-- /headers/ get their own module in the first place.
--
-- For example, @--select-except-deprecated@ excludes deprecated
-- declarations from every module, while @--except-library-root 'internal'@
-- prevents @internal.h@ from getting its own module altogether (though its
-- types remain available via program slicing).
--
-- Defaults to @BTrue@ (all declarations). Program slicing is overridden
-- per-step internally.
parseConfigLibrary :: Parser Config
parseConfigLibrary = Config
    <$> parseClangArgsConfig
    <*> parseBindingSpec
    <*> parseSelectionPredicateWith [BTrue]
    <*> pure DisableProgramSlicing
    <*> parseFieldNamingStrategy
    <*> parsePathStyle

parseLibraryRoot :: Parser FilePath
parseLibraryRoot = strOption $ mconcat [
      long "library-root"
    , metavar "DIR"
    , help $ concat [
          "Directory containing library headers (module-generation scope). "
        , "Only headers whose canonical path is under a library root "
        , "get their own Haskell module. Also determines the base path "
        , "for deriving module names. "
        , "Repeatable; at least one required. "
        , "This is NOT the clang search path (-I)."
        ]
    ]

parseExceptLibraryRoot :: Parser Regex
parseExceptLibraryRoot = strOption $ mconcat [
      long "except-library-root"
    , metavar "PCRE"
    , help $ concat [
          "Exclude headers matching PCRE from module generation, "
        , "even if they fall under a --library-root. "
        , "This is a module-generation scope filter, NOT a selection predicate: "
        , "types from excluded headers remain available via program slicing."
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

    roots <- mapM canonicalizePath opts.libraryRoots

    let allHeaders = IncludeGraph.toSortedList includeGraph
        filtered   = filterByLibraryRoot roots opts.exceptLibraryRoot allHeaders
        modules    = [ (h, deriveModuleName roots opts.baseModule h)
                     | h <- filtered.included
                     ]

    let checkCats = case opts.outputOpts.mode of
          FilePerModule -> True
          SingleFile{}  -> False

    case detectCollisions checkCats modules of
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

    eErr <- withTracer global.unsafe $ \tracer -> do
      let ppTracer = contramap TracePreprocessLibrary tracer
      executePlan global opts ppTracer roots (map fst modules)

    case eErr of
      Right () -> pure ()
      Left err -> do
        print $ prettyForTrace err
        exitWith (ExitFailure 3)

-- | Print a human-readable processing plan for @--dry-run@.
printPlan :: LibraryHeaderResult -> [(RealPath, BaseModuleName)] -> IO ()
printPlan filtered modules = do
    putStrLn $ concat [
        show (length modules), " modules to generate"
      , case length filtered.excluded of
          0 -> ""
          n -> " (" ++ show n ++ " excluded by --except-library-root)"
      ]
    putStrLn ""
    mapM_ (\(hdr, m) ->
      putStrLn $ "  " ++ getRealPath hdr ++ " -> " ++ Text.unpack m.text
      ) modules

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
  -> [FilePath]
  -- ^ Canonicalized library roots used for module naming
  -> [RealPath]
  -- ^ Library-root-filtered headers in dependency order
  -> IO ()
executePlan global opts tracer roots headers =
    withSystemTempDirectory "hs-bindgen-library" $ \tempDir ->
      foldM_ (executeStep global opts tracer roots tempDir) [] headers

-- | Run one step, producing a Haskell module and a binding spec.
--
-- Returns the updated list of binding spec paths (previous specs plus the
-- one generated by this step).
executeStep ::
     GlobalOpts
  -> Opts
  -> Tracer PreprocessLibraryMsg
  -> [FilePath]
  -- ^ Canonicalized library roots
  -> FilePath
  -- ^ Temporary directory for intermediate binding specs
  -> [FilePath]
  -- ^ Binding specs accumulated from previous steps
  -> RealPath
  -> IO [FilePath]
executeStep global opts tracer roots tempDir accSpecs hdr = do
    let modName    = deriveModuleName roots opts.baseModule hdr
        bsPath     = tempDir </> moduleToPath modName <.> "yaml"

        stepConfig = opts.config {
            selectionPredicate =
              BAnd (selectionFor hdr) opts.config.selectionPredicate
          , programSlicing     = EnableProgramSlicing
          , bindingSpec        = opts.config.bindingSpec {
                extBindingSpecs =
                  opts.config.bindingSpec.extBindingSpecs ++ accSpecs
              }
          }

        bindgenConfig = toBindgenConfig
          stepConfig
          opts.uniqueId
          modName
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
      PreprocessLibraryProcessing hdr (Text.unpack modName.text)

    createDirectoryIfMissing True (takeDirectory bsPath)
    hsBindgen global.unsafe global.safe bindgenConfig opts.inputs artefact

    pure (accSpecs ++ [bsPath])

{-------------------------------------------------------------------------------
  Selection predicates

  A selection predicate controls which C declarations get Haskell bindings
  in a given hsBindgen run. For preprocess-library, we construct a
  Boolean SelectionPredicate that matches declarations by the canonical
  path of the header they were declared in.

  This is distinct from module-generation scope (--library-root /
  --except-library-root), which determines which headers get their own
  module in the first place.

  The \\Q...\\E PCRE syntax quotes the path literally, so special
  characters (dots, slashes) need no escaping.

  Combined with EnableProgramSlicing, this ensures that:
  - Only declarations physically in the target header get bindings.
  - Types they depend on (even from other headers) are pulled in as
    needed for the generated code to compile.
-------------------------------------------------------------------------------}

-- | Build a selection predicate that accepts only declarations from the
-- given header path.
selectionFor :: RealPath -> Boolean SelectionPredicate
selectionFor path =
    BIf (SelectHeader (HeaderPathMatches (exactMatch path)))
  where
    -- clang_File_tryGetRealPathName uses forward slashes on all platforms,
    -- so the path never contains a literal \E that would break PCRE quoting.
    exactMatch :: RealPath -> Regex
    exactMatch (RealPath p) =
        fromString $ "^\\Q" ++ Text.unpack p ++ "\\E$"
