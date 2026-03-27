-- | @hs-bindgen-cli info doxygen@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.Doxygen qualified as Doxygen
module HsBindgen.Cli.Info.Doxygen (
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
import HsBindgen.ArtefactM
import HsBindgen.Backend.Category
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the parsed doxygen state"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config     :: Config
    , output     :: Maybe FilePath
    , inputs     :: [UncheckedHashIncludeArg]
    , filePolicy :: FilePolicy
    , dirPolicy  :: DirPolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> optional parseOutput'
      <*> parseInputs
      <*> parseFilePolicy
      <*> parseDirPolicy

parseOutput' :: Parser FilePath
parseOutput' = strOption $ mconcat [
      short 'o'
    , long "output"
    , metavar "PATH"
    , help "Output path for the doxygen state"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts =
    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact ()
    artefact = writeDoxygen opts.filePolicy opts.dirPolicy opts.output

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          (UniqueId       "unused-unique-id")
          (BaseModuleName "unused-module-name")
          (def :: ByCategory Choice)
