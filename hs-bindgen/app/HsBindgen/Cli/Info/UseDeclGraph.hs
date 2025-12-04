-- | @hs-bindgen-cli info use-decl-graph@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.UseDeclGraph qualified as UseDeclGraph
module HsBindgen.Cli.Info.UseDeclGraph (
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
import HsBindgen.Artefact
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the use-decl graph"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config              :: Config
    , uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , output              :: Maybe FilePath
    , inputs              :: [UncheckedHashIncludeArg]
    , fileOverwritePolicy :: FileOverwritePolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
      <*> optional parseOutput'
      <*> parseInputs
      <*> parseFileOverwritePolicy

parseOutput' :: Parser FilePath
parseOutput' = strOption $ mconcat [
      short 'o'
    , long "output"
    , metavar "PATH"
    , help "Output path for the graph"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefact = writeUseDeclGraph fileOverwritePolicy output
        bindgenConfig = toBindgenConfig config uniqueId baseModuleName
    void $ hsBindgen tracerConfig bindgenConfig inputs artefact
