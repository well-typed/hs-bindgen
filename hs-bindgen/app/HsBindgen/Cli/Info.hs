-- | @hs-bindgen-cli info@ commands
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info qualified as Info
module HsBindgen.Cli.Info (
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
import HsBindgen.Cli.Info.ResolveHeader qualified as ResolveHeader

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Informational commands, useful when creating bindings"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered lexicographically
newtype Cmd =
    CmdResolveHeader ResolveHeader.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd
        "resolve-header"
        CmdResolveHeader
        ResolveHeader.parseOpts
        ResolveHeader.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdResolveHeader opts -> ResolveHeader.exec gopts opts
