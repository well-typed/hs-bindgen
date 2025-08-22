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
import HsBindgen.Cli.Dev.BindingSpec qualified as BindingSpec
import HsBindgen.Cli.Dev.Clang qualified as Clang
import HsBindgen.Cli.Dev.Parse qualified as Parse
import HsBindgen.Cli.Dev.Resolve qualified as Resolve

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
    CmdBindingSpec BindingSpec.Cmd
  | CmdClang       Clang.Opts
  | CmdParse       Parse.Opts
  | CmdResolve     Resolve.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "binding-spec" CmdBindingSpec BindingSpec.parseCmd BindingSpec.info
    , cmd "clang"        CmdClang       Clang.parseOpts      Clang.info
    , cmd "parse"        CmdParse       Parse.parseOpts      Parse.info
    , cmd "resolve"      CmdResolve     Resolve.parseOpts    Resolve.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdBindingSpec cmd' -> BindingSpec.exec gopts cmd'
    CmdClang       opts -> Clang.exec       gopts opts
    CmdParse       opts -> Parse.exec       gopts opts
    CmdResolve     opts -> Resolve.exec     gopts opts
