-- | @hs-bindgen-cli info targets@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.Targets qualified as Targets
module HsBindgen.Cli.Info.Targets (
    -- * CLI help
    info
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)

import HsBindgen.Config.ClangArgs

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "List supported targets"

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: IO ()
exec = mapM_ (putStrLn . targetTriple) [minBound..]
