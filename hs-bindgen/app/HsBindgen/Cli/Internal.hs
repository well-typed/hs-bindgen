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
import HsBindgen.Cli.Internal.Frontend qualified as Frontend

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Internal commands, for hs-bindgen development"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered lexicographically
newtype Cmd =
    CmdFrontend Frontend.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "frontend" CmdFrontend Frontend.parseOpts Frontend.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdFrontend opts -> Frontend.exec gopts opts
