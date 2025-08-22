-- | @hs-bindgen-cli internal@ commands
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Internal qualified as Internal
module HsBindgen.Cli.Internal (
    -- * CLI help
    info
    -- * Commands
  , Cmd(..)
  , parseCmd
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen.App
import HsBindgen.Cli.Internal.Literate qualified as Literate

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Internal commands, not meant to be used directly"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | Ordered lexicographically
newtype Cmd =
    CmdLiterate Literate.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd_ "literate" CmdLiterate Literate.parseOpts Literate.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO (Maybe Literate.Opts)
exec _gopts = \case
    CmdLiterate opts -> return (Just opts)
