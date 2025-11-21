-- | @hs-bindgen-cli info libclang@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.Libclang qualified as Libclang
module HsBindgen.Cli.Info.Libclang (
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
import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config.ClangArgs hiding (getClangArgs)
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = mconcat [
      progDesc "Run libclang with empty input"
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
      clangArgsConfig :: ClangArgsConfig FilePath
    }

parseOpts :: Parser Opts
parseOpts = Opts <$> parseClangArgsConfig

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracer _ -> do
      clangArgs <- getClangArgs (contramap TraceBoot tracer) clangArgsConfig
      let hasNoUserOptions = hasNoUserClangOptions clangArgsConfig
          setup = defaultClangSetup clangArgs $
            ClangInputMemory "hs-bindgen-nop.h" ""

      -- Emit informational message if no user options provided
      when (   hasNoUserOptions
            && unwrapVerbosity (tVerbosity tracerConfig) >= Info) $ do
        traceWith (contramap (TraceFrontend . FrontendClang) tracer)
                  ClangInvokedWithoutOptions

      withClang
        (contramap (TraceFrontend . FrontendClang) tracer)
        setup
        (const (return Nothing))
  where
    -- Check if user provided any Clang options via command line
    hasNoUserClangOptions :: ClangArgsConfig FilePath -> Bool
    hasNoUserClangOptions ClangArgsConfig{..} =
      null argsBefore && null argsInner && null argsAfter
