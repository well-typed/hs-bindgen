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
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.App.Output (OutputMode (..), OutputOptions (..),
                             buildCategoryChoice, parseOutputOptions)
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.DelayedIO
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
    , uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , outputOptions       :: OutputOptions
    , hsOutputDir         :: FilePath
    , outputBindingSpec   :: Maybe FilePath
    , outputDirPolicy     :: OutputDirPolicy
    , fileOverwritePolicy :: FileOverwritePolicy
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs              :: [UncheckedHashIncludeArg]
    }
  deriving (Generic)

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
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
      opts.inputs
      artefact
  where
    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.uniqueId
          opts.baseModuleName
          (buildCategoryChoice opts.outputOptions)

    mrc :: ModuleRenderConfig
    mrc = ModuleRenderConfig {
        fieldNamingStrategy = opts.config.fieldNamingStrategy
      , qualifiedStyle      = opts.config.qualifiedStyle
      }

    artefact :: Artefact ()
    artefact = do
      case opts.outputOptions of
        OutputOptions (SingleFile _) ->
          writeBindingsSingleToDir
            mrc
            opts.fileOverwritePolicy
            opts.outputDirPolicy
            opts.hsOutputDir
        _                            ->
          writeBindingsMultiple
            mrc
            opts.fileOverwritePolicy
            opts.outputDirPolicy
            opts.hsOutputDir

      forM_ opts.outputBindingSpec $ \path ->
        writeBindingSpec opts.fileOverwritePolicy path
