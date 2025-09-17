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
import HsBindgen.Cli.Dev qualified as Dev
import HsBindgen.Cli.GenTests qualified as GenTests
import HsBindgen.Cli.Info qualified as Info
import HsBindgen.Cli.Internal qualified as Internal
import HsBindgen.Cli.Internal.Literate qualified as Literate
import HsBindgen.Cli.Preprocess qualified as Preprocess

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered by usage
data Cmd =
    CmdPreprocess Preprocess.Opts
  | CmdGenTests   GenTests.Opts
  | CmdInfo       Info.Cmd
  | CmdDev        Dev.Cmd
  | CmdInternal   Internal.Cmd

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd  "preprocess" CmdPreprocess Preprocess.parseOpts Preprocess.info
    , cmd  "gentests"   CmdGenTests   GenTests.parseOpts   GenTests.info
    , cmd  "info"       CmdInfo       Info.parseCmd        Info.info
    , cmd  "dev"        CmdDev        Dev.parseCmd         Dev.info
    , cmd_ "internal"   CmdInternal   Internal.parseCmd    Internal.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO (Maybe Literate.Opts)
exec gopts = \case
    CmdPreprocess opts -> Nothing <$ Preprocess.exec gopts opts
    CmdGenTests   opts -> Nothing <$ GenTests.exec   gopts opts
    CmdInfo       cmd' -> Nothing <$ Info.exec       gopts cmd'
    CmdDev        cmd' -> Nothing <$ Dev.exec        gopts cmd'
    CmdInternal   cmd' ->            Internal.exec   gopts cmd'
