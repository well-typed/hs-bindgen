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

import Data.Default (Default (..))
import Options.Applicative hiding (info)

import HsBindgen
import HsBindgen.App
import HsBindgen.Config
import HsBindgen.Config.Internal (BindgenConfig)
import HsBindgen.DelayedIO
import HsBindgen.Frontend.RootHeader
import HsBindgen.FrontendDump

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Dump the result of a frontend pass"

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

exec :: Show result => GlobalOpts -> Opts -> FrontendDump result -> IO ()
exec global opts pass =
    hsBindgen
      global.unsafe
      global.safe
      bindgenConfig
      opts.inputs
      artefact
  where
    artefact :: Artefact ()
    artefact = do
        result <- RunFrontendDump pass
        Lift $ delay . WriteToStdOut . StringContent $ show result

    bindgenConfig :: BindgenConfig
    bindgenConfig =
        toBindgenConfig
          opts.config
          opts.uniqueId
          opts.baseModuleName
          def
