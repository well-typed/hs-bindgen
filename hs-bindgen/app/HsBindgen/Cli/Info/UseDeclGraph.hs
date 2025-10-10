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

import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

import HsBindgen

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the use-decl graph"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config       :: Config
    , configPP     :: ConfigPP
    , output       :: Maybe FilePath
    , inputs       :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigPP
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
    let artefacts = writeUseDeclGraph output :* Nil
        bindgenConfig = toBindgenConfigPP config configPP
    void $ hsBindgen tracerConfig bindgenConfig inputs artefacts
