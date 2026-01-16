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
import HsBindgen.Cli.Info.BuiltinMacros qualified as BuiltinMacros
import HsBindgen.Cli.Info.IncludeGraph qualified as IncludeGraph
import HsBindgen.Cli.Info.Libclang qualified as Libclang
import HsBindgen.Cli.Info.ResolveHeader qualified as ResolveHeader
import HsBindgen.Cli.Info.UseDeclGraph qualified as UseDeclGraph

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Informational commands, useful when creating bindings"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- Ordered lexicographically
data Cmd =
    CmdBuiltinMacros BuiltinMacros.Opts
  | CmdIncludeGraph  IncludeGraph.Opts
  | CmdLibclang      Libclang.Opts
  | CmdResolveHeader ResolveHeader.Opts
  | CmdUseDeclGraph  UseDeclGraph.Opts

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd
        "builtin-macros"
        CmdBuiltinMacros
        BuiltinMacros.parseOpts
        BuiltinMacros.info
    , cmd
        "include-graph"
        CmdIncludeGraph
        IncludeGraph.parseOpts
        IncludeGraph.info
    , cmd
        "libclang"
        CmdLibclang
        Libclang.parseOpts
        Libclang.info
    , cmd
        "resolve-header"
        CmdResolveHeader
        ResolveHeader.parseOpts
        ResolveHeader.info
    , cmd
        "use-decl-graph"
        CmdUseDeclGraph
        UseDeclGraph.parseOpts
        UseDeclGraph.info
    ]

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdBuiltinMacros opts -> BuiltinMacros.exec gopts opts
    CmdIncludeGraph  opts -> IncludeGraph.exec  gopts opts
    CmdLibclang      opts -> Libclang.exec      gopts opts
    CmdResolveHeader opts -> ResolveHeader.exec gopts opts
    CmdUseDeclGraph  opts -> UseDeclGraph.exec  gopts opts
