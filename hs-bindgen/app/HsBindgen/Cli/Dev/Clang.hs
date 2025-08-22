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

import HsBindgen.Clang
import HsBindgen.Imports
import HsBindgen.Lib
import HsBindgen.Util.Tracer (natTracer)

import HsBindgen.App

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
      clangArgs :: ClangArgs
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracerM ->
      let tracer = contramap (TraceFrontend. FrontendClang) $
            natTracer id tracerM
      in  void $ withClang tracer setup (const (return Nothing))
  where
    setup :: ClangSetup
    setup = defaultClangSetup clangArgs $ ClangInputMemory "hs-bindgen-nop.h" ""
