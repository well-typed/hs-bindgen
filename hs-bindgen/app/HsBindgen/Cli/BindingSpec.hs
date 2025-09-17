-- | @hs-bindgen-cli binding-spec@ commands
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.BindingSpec qualified as BindingSpec
module HsBindgen.Cli.BindingSpec (
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
import HsBindgen.Cli.BindingSpec.StdLib qualified as StdLib

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Binding specification commands"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered lexicographically
newtype Cmd =
    CmdStdLib StdLib.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "stdlib" CmdStdLib StdLib.parseOpts StdLib.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdStdLib opts -> StdLib.exec gopts opts
