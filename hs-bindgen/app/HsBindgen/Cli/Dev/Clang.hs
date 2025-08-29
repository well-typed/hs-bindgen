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

import Prettyprinter.Util qualified as PP

import Options.Applicative hiding (info)

import HsBindgen.Clang
import HsBindgen.Clang.BuiltinIncDir
import HsBindgen.Imports
import HsBindgen.Lib

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

data Opts = Opts {
      builtinIncDirConfig :: BuiltinIncDirConfig
    , clangArgs           :: ClangArgs
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseBuiltinIncDirConfig
      <*> parseClangArgs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec GlobalOpts{..} Opts{..} =
    void . withTracer tracerConfig $ \tracer -> do
      clangArgs' <- applyBuiltinIncDir clangArgs <$>
        getBuiltinIncDir
          (contramap TraceBuiltinIncDir tracer)
          builtinIncDirConfig
      let setup = defaultClangSetup clangArgs' $
            ClangInputMemory "hs-bindgen-nop.h" ""
      withClang
        (contramap (TraceFrontend . FrontendClang) tracer)
        setup
        (const (return Nothing))
