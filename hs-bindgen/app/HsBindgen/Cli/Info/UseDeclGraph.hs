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
import HsBindgen.Backend.UniqueId
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
import HsBindgen.Language.Haskell qualified as Hs

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
    , uniqueId     :: UniqueId
    , hsModuleName :: Hs.ModuleName
    , output       :: Maybe FilePath
    , inputs       :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseHsModuleName
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
    let artefact = writeUseDeclGraph output
        bindgenConfig = toBindgenConfig config uniqueId hsModuleName
    void $ hsBindgen tracerConfig bindgenConfig inputs artefact
