-- | @hs-bindgen-cli dev@ commands
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Dev qualified as Dev
module HsBindgen.Cli.Dev (
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
import HsBindgen.Cli.Dev.Clang qualified as Clang
import HsBindgen.Cli.Dev.Parse qualified as Parse

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Development commands, used for debugging"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered lexicographically
data Cmd =
    CmdClang Clang.Opts
  | CmdParse Parse.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "clang" CmdClang Clang.parseOpts Clang.info
    , cmd "parse" CmdParse Parse.parseOpts Parse.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdClang opts -> Clang.exec gopts opts
    CmdParse opts -> Parse.exec gopts opts
