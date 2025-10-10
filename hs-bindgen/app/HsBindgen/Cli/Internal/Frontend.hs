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
import HsBindgen.Config
import HsBindgen.Frontend.RootHeader
import HsBindgen.Language.Haskell qualified as Hs

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Parse C headers (all Frontend passes)"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      config       :: Config
    , configPP     :: ConfigPP
    , hsModuleName :: Hs.ModuleName
    , inputs       :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseConfigPP
      <*> parseHsModuleName
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefacts = ReifiedC :* Nil
        bindgenConfig = toBindgenConfigPP config configPP
    (I decls :* Nil) <-
      hsBindgen tracerConfig bindgenConfig hsModuleName inputs artefacts
    print decls
