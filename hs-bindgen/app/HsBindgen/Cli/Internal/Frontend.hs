-- | @hs-bindgen-cli internal frontend@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Internal.Frontend qualified as Frontend
module HsBindgen.Cli.Internal.Frontend (
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

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Parse C headers (all Frontend passes)"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config              :: Config
    , uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , inputs              :: [UncheckedHashIncludeArg]
    , fileOverwritePolicy :: FileOverwritePolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
      <*> parseInputs
      <*> parseFileOverwritePolicy

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefact = ReifiedC >>= Lift . delay . PutStrLn . show
        bindgenConfig = toBindgenConfig config uniqueId baseModuleName
    hsBindgen tracerConfig bindgenConfig inputs artefact
