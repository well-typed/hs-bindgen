-- | @hs-bindgen-cli@ commands
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli qualified as Cli
module HsBindgen.Cli (
    -- * Commands
    Cmd(..)
  , parseCmd
    -- * Execution
  , exec
  ) where

import Options.Applicative

import HsBindgen.App
import HsBindgen.Cli.BindingSpec qualified as BindingSpec
import HsBindgen.Cli.GenTests qualified as GenTests
import HsBindgen.Cli.Info qualified as Info
import HsBindgen.Cli.Internal qualified as Internal
import HsBindgen.Cli.Preprocess qualified as Preprocess
import HsBindgen.Cli.ToolSupport qualified as ToolSupport

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered by usage
data Cmd =
    CmdPreprocess  Preprocess.Opts
  | CmdGenTests    GenTests.Opts
  | CmdBindingSpec BindingSpec.Cmd
  | CmdInfo        Info.Cmd
  | CmdInternal    Internal.Cmd
  | CmdToolSupport ToolSupport.Cmd

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd  "preprocess"   CmdPreprocess  Preprocess.parseOpts Preprocess.info
    , cmd  "gen-tests"    CmdGenTests    GenTests.parseOpts   GenTests.info
    , cmd  "binding-spec" CmdBindingSpec BindingSpec.parseCmd BindingSpec.info
    , cmd  "info"         CmdInfo        Info.parseCmd        Info.info
    , cmd  "internal"     CmdInternal    Internal.parseCmd    Internal.info
    , cmd_ "tool-support" CmdToolSupport ToolSupport.parseCmd ToolSupport.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdPreprocess  opts -> Preprocess.exec  gopts opts
    CmdGenTests    opts -> GenTests.exec    gopts opts
    CmdBindingSpec cmd' -> BindingSpec.exec gopts cmd'
    CmdInfo        cmd' -> Info.exec        gopts cmd'
    CmdInternal    cmd' -> Internal.exec    gopts cmd'
    CmdToolSupport cmd' -> ToolSupport.exec gopts cmd'
