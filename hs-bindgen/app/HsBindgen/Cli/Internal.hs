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
import HsBindgen.FrontendDump

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Internal commands, for hs-bindgen development"

{-------------------------------------------------------------------------------
  Commands
-------------------------------------------------------------------------------}

-- | Internal commands
--
-- 'CmdDumpPass' uses an existential to pair a 'FrontendDump' with a 'Show'
-- constraint on its result type, so that we can dump the result of any pass.
data Cmd where
  CmdDumpPass ::
       Show result
    => Frontend.Opts
    -> FrontendDump result
    -> Cmd

parseCmd :: Parser Cmd
parseCmd = subparser $ mconcat [
      cmd "frontend" id parseFrontendCmd Frontend.info
    ]

-- | Parse a @frontend@ sub-command selecting a specific pass to dump
parseFrontendCmd :: Parser Cmd
parseFrontendCmd = subparser $ mconcat [
      passCmd DumpParse
              "Dump after Parse pass"
    , passCmd DumpSimplifyAST
              "Dump after SimplifyAST pass"
    , passCmd DumpAssignAnonIds
              "Dump after AssignAnonIds pass"
    , passCmd DumpConstructTranslationUnit
              "Dump after ConstructTranslationUnit pass"
    , passCmd DumpHandleMacros
              "Dump after HandleMacros pass"
    , passCmd DumpResolveBindingSpecs
              "Dump after ResolveBindingSpecs pass"
    , passCmd DumpMangleNames
              "Dump after MangleNames pass"
    , passCmd DumpSelect
              "Dump after Select pass"
    , passCmd DumpAdjustTypes
              "Dump after AdjustTypes (final) pass"
    ]
  where
    passCmd ::
         Show result
      => FrontendDump result
      -> String
      -> Mod CommandFields Cmd
    passCmd pass desc =
        cmd (frontendDumpName pass) (`CmdDumpPass` pass) Frontend.parseOpts (progDesc desc)

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Cmd -> IO ()
exec gopts = \case
    CmdDumpPass opts pass -> Frontend.exec gopts opts pass
