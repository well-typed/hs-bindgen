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
import System.Exit (exitFailure)

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
    Opts
      <$> parseConfig
      <*> parseConfigCLI
      <*> PreprocessLibrary.parseOpts

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
    when opts.configLibrary.listModules $ do
      putStrLn "Error: --list-modules requires --library"
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
    when (isJust opts.configCLI.outputBindingSpec) $ do
      putStrLn "Error: --gen-binding-spec cannot be used with --library"
      exitFailure

    PreprocessLibrary.exec global
      (libraryDefaults opts.config)
      opts.configCLI.uniqueId
      opts.configCLI.baseModuleName
      opts.configCLI.qualifiedStyle
      opts.configCLI.outputOptions
      opts.configCLI.hsOutputDir
      opts.configCLI.dirPolicy
      opts.configCLI.filePolicy
      opts.configCLI.inputs
      opts.configLibrary

-- | Adjust the selection predicate default for library mode.
--
-- 'parseConfig' defaults to @FromMainHeaders@ (select declarations from the
-- root header only). In library mode each step ANDs the user predicate with
-- a per-header filter, so @FromMainHeaders@ would intersect with a sub-header
-- filter and produce nothing. We replace it with @BTrue@ so the per-header
-- filter does the actual selection. Explicit @--select-*@ flags are not
-- affected since they do not contain @FromMainHeaders@.
libraryDefaults :: Config -> Config
libraryDefaults config = config {
      selectionPredicate = go config.selectionPredicate
    }
  where
    go :: Boolean SelectionPredicate -> Boolean SelectionPredicate
    go = \case
      BIf (SelectHeader FromMainHeaders) -> BTrue
      BAnd a b -> BAnd (go a) (go b)
      BOr  a b -> BOr  (go a) (go b)
      BNot a   -> BNot (go a)
      other    -> other
