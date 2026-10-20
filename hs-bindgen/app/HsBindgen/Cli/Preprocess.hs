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

import GHC.Exception (Exception (..))
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Errors
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Macro

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Generate Haskell module from C headers"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config              :: Config
    , configCLI           :: ConfigCLI
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigCLI

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
    , hsBootPolicy      :: HsBootPolicy
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
      <*> parseHsBootPolicy
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
    case (opts.configCLI.outputOptions, opts.configCLI.hsBootPolicy) of
      (OutputOptions (SingleFile _), GenerateHsBoot) ->
        throwIO HsBootWithSingleFile
      _otherwise ->
        pure ()
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
          opts.configCLI.hsBootPolicy

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

{-------------------------------------------------------------------------------
  Exception
-------------------------------------------------------------------------------}

-- | Options that @preprocess@ cannot combine
data PreprocessException = HsBootWithSingleFile
  deriving Show

instance Exception PreprocessException where
    toException = hsBindgenExceptionToException
    fromException = hsBindgenExceptionFromException
    displayException HsBootWithSingleFile =
      "--generate-hs-boot is not available with --single-file"
