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

import Control.Monad (void)
import Prettyprinter.Util qualified as PP

import Options.Applicative hiding (info)

import HsBindgen.Lib

import HsBindgen.App

-- NOTE: Internal API.
import HsBindgen.Clang (ClangInput (..), ClangSetup, defaultClangSetup,
                        withClang)
import HsBindgen.Util.Tracer (natTracer)

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
