-- | @hs-bindgen-cli info include-graph@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.IncludeGraph qualified as IncludeGraph
module HsBindgen.Cli.Info.IncludeGraph (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen.App
import HsBindgen.Imports
import HsBindgen.Lib

import HsBindgen (writeIncludeGraph)

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the include graph"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      bindgenConfig :: BindgenConfig
    , output        :: Maybe FilePath
    , inputs        :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBindgenConfig
      <*> optional parseOutput'
      <*> parseInputs

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
    let artefacts = writeIncludeGraph output :* Nil
    void $ hsBindgen tracerConfig tracerConfigBackend bindgenConfig inputs artefacts
