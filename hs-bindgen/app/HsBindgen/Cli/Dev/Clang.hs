-- | @hs-bindgen-cli dev clang@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Dev.Clang qualified as Clang
module HsBindgen.Cli.Dev.Clang (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Options.Applicative hiding (info)
import Prettyprinter.Util qualified as PP

import HsBindgen.App
import HsBindgen.Boot (getClangArgs)
import HsBindgen.Clang
import HsBindgen.Imports
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = mconcat [
      progDesc "Run Clang with empty input"
    , footerDoc . Just . PP.reflow $ mconcat [
          "This command provides a way to get output from libclang."
        , " For example, use --clang-option=-v to see version and include"
        , " search path information, taking into account any other Clang"
        , " options and environment variables."
        ]
    ]

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

newtype Opts = Opts {
      clangArgsConfig :: ClangArgsConfig
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgsConfig

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracer -> do
      clangArgs <- getClangArgs (contramap TraceBoot tracer) clangArgsConfig
      let setup = defaultClangSetup clangArgs $
            ClangInputMemory "hs-bindgen-nop.h" ""
      withClang
        (contramap (TraceFrontend . FrontendClang) tracer)
        setup
        (const (return Nothing))
