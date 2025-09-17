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
import HsBindgen.Cli.Internal.Parse qualified as Parse

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
    CmdParse Parse.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "parse" CmdParse Parse.parseOpts Parse.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdParse opts -> Parse.exec gopts opts
