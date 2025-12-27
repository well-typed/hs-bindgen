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

import HsBindgen
import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.DelayedIO
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Output the include graph"

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
exec global opts =
    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact ()
    artefact = writeIncludeGraph opts.fileOverwritePolicy opts.output

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.uniqueId
          opts.baseModuleName
          def
