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

import Control.Monad.IO.Class
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Backend.UniqueId
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
    , uniqueId     :: UniqueId
    , hsModuleName :: Hs.ModuleName
    , inputs       :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseConfig
      <*> parseUniqueId
      <*> parseHsModuleName
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} = do
    let artefact = ReifiedC >>= liftIO . print
        bindgenConfig = toBindgenConfig config uniqueId hsModuleName
    hsBindgen tracerConfig bindgenConfig inputs artefact
