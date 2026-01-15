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
import System.Exit (exitFailure)
import Text.SimplePrettyPrint qualified as PP

import Clang.Enum.Simple (SimpleEnum (..), simpleFromC)
import Clang.LowLevel.Core (CXErrorCode (..))

import HsBindgen.App
import HsBindgen.Boot
import HsBindgen.Clang
import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = mconcat [
      progDesc "Run libclang with empty input"
    , footerDoc . Just . PP.reflow $ mconcat infoHelpMessage
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
exec global opts = do
    eRes <- withTracer tracerConfigWithoutASTReadError $ \tracer -> do
      clangArgs <-
        getClangArgs (contramap TraceBoot tracer) opts.clangArgsConfig
      let hasNoUserOptions = hasNoUserClangOptions opts.clangArgsConfig
          setup = defaultClangSetup clangArgs $
            ClangInputMemory "hs-bindgen-nop.h" ""

      -- Emit informational message if no user options provided
      when (   hasNoUserOptions
            && global.unsafe.verbosity.level >= Info) $ do
        traceWith (contramap (TraceFrontend . FrontendClang) tracer)
                  ClangInvokedWithoutOptions

      withClang
        (contramap (TraceFrontend . FrontendClang) tracer)
        setup
        (const (return Nothing))
    case eRes of
      Left e  -> do
        putStrLn $ PP.renderCtxDoc PP.defaultContext $ prettyForTrace e
        exitFailure
      Right _ -> pure ()
  where
    -- Check if user provided any Clang options via command line
    --
    hasNoUserClangOptions :: ClangArgsConfig FilePath -> Bool
    hasNoUserClangOptions clangArgs = and [
          null clangArgs.argsBefore
        , null clangArgs.argsInner
        , null clangArgs.argsAfter
        ]

    -- Check if a trace is CXError_ASTReadError.
    -- This error is benign in the context of 'info libclang' when users
    -- provide diagnostic flags, as they cause Clang to exit without building an AST.
    --
    -- If trace is CXError_ASTReadError make it debug level which should
    -- supress it but still make it available if needed with Debug level
    -- logging
    --
    tracerConfigWithoutASTReadError =
      global.unsafe {
          customLogLevel = CustomLogLevel $ \trace actualLevel ->
            case trace of
              TraceFrontend (FrontendClang (ClangErrorCode (SimpleEnum x)))
                | Just CXError_ASTReadError <- simpleFromC x ->
                  Debug
              _ ->
                applyCustomLogLevel
                  global.unsafe.customLogLevel
                  trace
                  actualLevel
        }
