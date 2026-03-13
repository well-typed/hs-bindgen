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

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.ArtefactM
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

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
      uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , qualifiedStyle      :: QualifiedStyle
    , outputOptions       :: OutputOptions
    , hsOutputDir         :: FilePath
    , outputBindingSpec   :: Maybe FilePath
    , outputDirPolicy     :: OutputDirPolicy
    , fileOverwritePolicy :: FileOverwritePolicy
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs              :: [UncheckedHashIncludeArg]
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
      <*> parseOutputDirPolicy
      <*> parseFileOverwritePolicy
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
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

    artefact :: Artefact ()
    artefact = do
      case opts.configCLI.outputOptions of
        OutputOptions (SingleFile _) ->
          writeBindingsSingleToDir
            mrc
            opts.configCLI.fileOverwritePolicy
            opts.configCLI.outputDirPolicy
            opts.configCLI.hsOutputDir
        _ ->
          writeBindingsMultiple
            mrc
            opts.configCLI.fileOverwritePolicy
            opts.configCLI.outputDirPolicy
            opts.configCLI.hsOutputDir

      forM_ opts.configCLI.outputBindingSpec $ \path ->
        writeBindingSpec opts.configCLI.fileOverwritePolicy path
