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
import System.Directory (doesDirectoryExist)
import System.Exit (ExitCode (..), exitWith)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions,
                             buildCategoryChoice, parseOutputOptions,
                             writeBindingsWith)
import HsBindgen.ArtefactM
import HsBindgen.Cli.Preprocess.Library qualified as Library
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.Frontend.Predicate
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc $ concat [
    "Generate Haskell module from C headers. "
  , "Use --library to generate one module per header "
  , "of a multi-header C library."
  ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config        :: Config
    , configCLI     :: ConfigCLI
    , configLibrary :: Library.Opts
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    mk
      <$> parseConfigWithDefault
      <*> Library.parseOpts
      <*> parseConfigCLI
  where
    -- In library mode each step already narrows the selection to the headers
    -- of one module, so by default it selects every declaration in them.
    --
    -- The library options are parsed before 'ConfigCLI', whose inputs have to
    -- come last (see there).
    mk ::
         ([Boolean SelectionPredicate] -> Config)
      -> Library.Opts
      -> ConfigCLI
      -> Opts
    mk config configLibrary configCLI =
        Opts (config defaultPositives) configCLI configLibrary
      where
        defaultPositives :: [Boolean SelectionPredicate]
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
    | otherwise                                  = execSingleModule global opts

execSingleModule :: GlobalOpts -> Opts -> IO ()
execSingleModule global opts = do
    when opts.configLibrary.dryRun $
      usageError "--dry-run requires --library"
    when opts.configLibrary.listBaseModuleNames $
      usageError "--list-base-module-names requires --library"

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
      writeBindingsWith
        opts.configCLI.outputOptions
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
    -- A directory that does not exist has no headers under it, so the run
    -- would generate nothing and still succeed.
    forM_ opts.configLibrary.libraryRoots $ \dir -> do
      exists <- doesDirectoryExist dir
      unless exists $
        usageError $ "--library is not a directory: " ++ dir

    Library.exec global runOpts opts.configLibrary
  where
    runOpts :: Library.RunOpts
    runOpts = Library.RunOpts {
          config         = opts.config
        , uniqueId       = opts.configCLI.uniqueId
        , baseModuleName = opts.configCLI.baseModuleName
        , qualifiedStyle = opts.configCLI.qualifiedStyle
        , outputOptions  = opts.configCLI.outputOptions
        , hsOutputDir    = opts.configCLI.hsOutputDir
        , dirPolicy      = opts.configCLI.dirPolicy
        , filePolicy     = opts.configCLI.filePolicy
        , inputs         = opts.configCLI.inputs
        }

-- | Report flags that cannot be used together, and exit with the code for
-- usage errors
usageError :: String -> IO a
usageError msg = do
    putStrLn $ "Error: " ++ msg
    exitWith (ExitFailure 2)
