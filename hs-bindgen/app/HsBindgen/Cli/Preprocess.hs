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
    -- Build category choice from options
    categoryChoice <- case buildCategoryChoice opts.categoryOptions of
      Left err     -> throwIO (userError err)
      Right choice -> return choice

    let bindgenConfig :: BindgenConfig
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
    -- When a specific category is selected, use single-module mode to combine
    -- all included categories (types, terms, globals) into one file with one
    -- addCSource directive.  This avoids duplicate symbol errors with
    -- header-only libraries.
    --
    -- When no category is specified (default), use multi-module mode for
    -- backwards compatibility.
    artefact :: Artefact ()
    artefact = do
        if usesSingleModule opts.categoryOptions
          then writeBindingsSingleToDir
                 opts.fileOverwritePolicy
                 opts.outputDirPolicy
                 opts.hsOutputDir
          else writeBindingsMultiple
                 opts.fileOverwritePolicy
                 opts.outputDirPolicy
                 opts.hsOutputDir
        forM_ opts.outputBindingSpec $ \path ->
          writeBindingSpec opts.fileOverwritePolicy path

    -- Check if a specific category was selected (use single-module mode)
    -- vs using default (all categories, use multi-module mode)
    usesSingleModule :: CategoryOptions -> Bool
    usesSingleModule catOpts = not (null catOpts.selections)
