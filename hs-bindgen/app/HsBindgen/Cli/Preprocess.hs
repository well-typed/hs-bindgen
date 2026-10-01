-- | @hs-bindgen-cli preprocess@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Preprocess qualified as Preprocess
module HsBindgen.Cli.Preprocess (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
  , ConfigCLI(..)
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)
import System.Exit (ExitCode (..), exitFailure, exitWith)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Cli.PreprocessLibrary qualified as PreprocessLibrary
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ concat [
    "Generate Haskell module from C headers. "
  , "Use --library to generate one module per sub-header "
  , "for a multi-header C library."
  ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config        :: Config
    , configCLI     :: ConfigCLI
    , configLibrary :: PreprocessLibrary.Opts
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    mk
      <$> parseConfigWithDefault
      <*> parseConfigCLI
      <*> PreprocessLibrary.parseOpts
  where
    -- In library mode each step already narrows the selection to the headers
    -- of one module, so by default it selects every declaration in them.
    mk config configCLI configLibrary =
        Opts (config defaultPositives) configCLI configLibrary
      where
        defaultPositives
          | null configLibrary.libraryRoots = [def]
          | otherwise                       = [BTrue]

-- | CLI options; the TH equivalent of ConfigCLI' is 'HsBindgen.Config.ConfigTH'.
data ConfigCLI = ConfigCLI {
      uniqueId          :: UniqueId
    , baseModuleName    :: BaseModuleName
    , qualifiedStyle    :: QualifiedStyle
    , outputOptions     :: OutputOptions
    , hsOutputDir       :: FilePath
    , outputBindingSpec :: Maybe FilePath
    , dirPolicy         :: DirPolicy
    , filePolicy        :: FilePolicy
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs            :: [C.UncheckedRootDirective]
    }
  deriving (Generic)

parseConfigCLI :: Parser ConfigCLI
parseConfigCLI =
    ConfigCLI
      <$> parseUniqueId
      <*> parseBaseModuleName
      <*> parseQualifiedStyle
      <*> parseOutputOptions FilePerModule
      <*> parseHsOutputDir
      <*> optional parseGenBindingSpec
      <*> parseDirPolicy
      <*> parseFilePolicy
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts
    | not (null opts.configLibrary.libraryRoots) = execLibrary global opts
    | otherwise                                  = execSingleHeader global opts

execSingleHeader :: GlobalOpts -> Opts -> IO ()
execSingleHeader global opts = do
    when opts.configLibrary.dryRun $ do
      putStrLn "Error: --dry-run requires --library"
      exitFailure
    when opts.configLibrary.listBaseModuleNames $ do
      putStrLn "Error: --list-base-module-names requires --library"
      exitFailure
    when (isJust opts.configLibrary.genBindingSpecDir) $ do
      putStrLn "Error: --gen-binding-spec-dir requires --library"
      exitFailure

    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.configCLI.inputs
      artefact
  where
    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.configCLI.uniqueId
          opts.configCLI.baseModuleName
          (buildCategoryChoice opts.configCLI.outputOptions)

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
        qualifiedStyle = opts.configCLI.qualifiedStyle
      }

    artefact :: Artefact CExpr ()
    artefact = do
      case opts.configCLI.outputOptions of
        OutputOptions (SingleFile _) ->
          writeBindingsSingle
            mrc
            opts.configCLI.filePolicy
            opts.configCLI.dirPolicy
            opts.configCLI.hsOutputDir
        _ ->
          writeBindingsMultiple
            mrc
            opts.configCLI.filePolicy
            opts.configCLI.dirPolicy
            opts.configCLI.hsOutputDir

      forM_ opts.configCLI.outputBindingSpec $ \path ->
        writeBindingSpec
          opts.configCLI.filePolicy
          opts.configCLI.dirPolicy
          path

execLibrary :: GlobalOpts -> Opts -> IO ()
execLibrary global opts = do
    -- The library-mode default has no header predicate, so any header
    -- predicate here was passed on the command line.
    when (any isHeaderPredicate opts.config.selectionPredicate) $ do
      putStrLn $ concat [
          "Error: header selection predicates (--select-from-main-headers, "
        , "--select-from-main-header-dirs, --select-by-header-path, "
        , "--select-except-by-header-path) cannot be used with --library; "
        , "use --except-library to leave headers out"
        ]
      exitWith (ExitFailure 2)

    when (isJust opts.configCLI.outputBindingSpec) $
      void $ withTracer global.unsafe $ \tracer ->
        traceWith tracer $ withCallStack $
          TracePreprocessLibrary PreprocessLibraryGenBindingSpecIgnored

    PreprocessLibrary.exec global
      opts.config
      opts.configCLI.uniqueId
      opts.configCLI.baseModuleName
      opts.configCLI.qualifiedStyle
      opts.configCLI.outputOptions
      opts.configCLI.hsOutputDir
      opts.configCLI.dirPolicy
      opts.configCLI.filePolicy
      opts.configCLI.inputs
      opts.configLibrary

isHeaderPredicate :: SelectionPredicate -> Bool
isHeaderPredicate = \case
    SelectHeader{} -> True
    SelectDecl{}   -> False
