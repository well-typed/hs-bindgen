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
import HsBindgen.Backend.Category (ByCategory, Choice)
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
    , categoryOptions     :: CategoryOptions
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
      <*> parseCategoryOptions
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
    let categoryChoice :: ByCategory Choice
        categoryChoice = buildCategoryChoice opts.categoryOptions

        bindgenConfig :: BindgenConfig
        bindgenConfig =
            toBindgenConfig
              opts.config
              opts.uniqueId
              opts.baseModuleName
              categoryChoice

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
          (not (null opts.categoryOptions.selections))
        forM_ opts.outputBindingSpec $ \path ->
          writeBindingSpec opts.fileOverwritePolicy path
