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

import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader

import HsBindgen

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Parse C headers (all Frontend passes)"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config   :: Config
    , configPP :: ConfigPP
    , inputs   :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigPP
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefacts = ReifiedC :* Nil
        bindgenConfig = toBindgenConfigPP config configPP
    (I decls :* Nil) <-
      hsBindgen tracerConfig bindgenConfig inputs artefacts
    print decls
