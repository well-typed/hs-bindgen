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
import HsBindgen.Config
import HsBindgen.Config.Internal
import HsBindgen.DelayedIO
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.App.Output (OutputOptions (..), OutputMode (..), parseOutputOptions, buildCategoryChoice)

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
    -- NOTE: Inputs (arguments) must be last, options must go before it.
    , inputs              :: [UncheckedHashIncludeArg]
    , outputDirPolicy     :: OutputDirPolicy
    , fileOverwritePolicy :: FileOverwritePolicy
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
      <*> parseInputs
      <*> parseOutputDirPolicy
      <*> parseFileOverwritePolicy

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
    let bindgenConfig :: BindgenConfig
        bindgenConfig =
            toBindgenConfig
              opts.config
              opts.uniqueId
              opts.baseModuleName
              (buildCategoryChoice opts.outputOptions)

    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact ()
    artefact = do
        writeBindingsToDir
          opts.fileOverwritePolicy
          opts.outputDirPolicy
          opts.hsOutputDir
          (case opts.outputOptions of
            OutputOptions (SingleFile _) -> True
            _                            -> False
          )
        forM_ opts.outputBindingSpec $ \path ->
          writeBindingSpec opts.fileOverwritePolicy path
