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
      dump                :: SomeFrontendDump
    , config              :: Config
    , uniqueId            :: UniqueId
    , baseModuleName      :: BaseModuleName
    , inputs              :: [UncheckedHashIncludeArg]
    , fileOverwritePolicy :: FileOverwritePolicy
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseDump
      <*> parseConfig
      <*> parseUniqueId
      <*> parseBaseModuleName
      <*> parseInputs
      <*> parseFileOverwritePolicy

parseDump :: Parser SomeFrontendDump
parseDump = option (eitherReader parseFrontendDumpName) $ mconcat [
      long "pass"
    , value (SomeFrontendDump DumpAdjustTypes)
    , showDefaultWith (\(SomeFrontendDump d) -> frontendDumpName d)
    , help "Frontend pass to dump"
    , metavar "PASS"
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = case opts.dump of
    SomeFrontendDump pass ->
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
