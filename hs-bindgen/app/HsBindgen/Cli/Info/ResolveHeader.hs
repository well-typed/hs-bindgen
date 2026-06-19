-- | @hs-bindgen-cli info resolve-header@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.ResolveHeader qualified as ResolveHeader
module HsBindgen.Cli.Info.ResolveHeader (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Options.Applicative hiding (info)
import System.Exit (ExitCode (ExitFailure))

import HsBindgen.App
import HsBindgen.Boot
import HsBindgen.Clang.Macos
import HsBindgen.Config.ClangArgs
import HsBindgen.Imports
import HsBindgen.IR.C qualified as C
import HsBindgen.Resolve
import HsBindgen.TraceMsg
import HsBindgen.Util.Tracer

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Resolve C headers to source paths"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      clangArgsConfig :: ClangArgsConfig FilePath
    , inputs          :: [C.UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseClangArgsConfig
      <*> parseInputs

{-------------------------------------------------------------------------------
  Execution
-------------------------------------------------------------------------------}

exec :: GlobalOpts -> Opts -> IO ()
exec global opts = do
    eErr <- withTracer tracerConfig' $ \tracer -> do
      hashIncludeArgs <- checkInputs tracer opts.inputs
      checkMacosEnv (contramap (TraceBoot . BootMacos) tracer)
      clangArgs <- (.clangArgs) <$>
        getClangArtefacts (contramap TraceBoot tracer) opts.clangArgsConfig
      includes <-
        resolveHeaders
          (contramap TraceResolveHeader tracer)
          clangArgs
          (Set.fromList hashIncludeArgs)
      fmap or . forM hashIncludeArgs $ \header' ->
        case Map.lookup header' includes of
          Just path -> (False <$) . putStrLn $ concat [
              "#include <"
            , header'.path
            , "> resolved to "
            , show path
            ]
          Nothing   -> (True  <$) . putStrLn $ concat [
              "#include <"
            , header'.path
            , "> could not be resolved (header not found)"
            ]
    case eErr of
      Right False -> return ()
      Right True  -> throwIO (ExitFailure 1)
      Left e      -> do
        print $ prettyForTrace e
  where
    tracerConfig' :: TracerConfig Level TraceMsg
    tracerConfig' = global.unsafe{
        customLogLevel = customLogLevel <> global.unsafe.customLogLevel
      }

    customLogLevel :: CustomLogLevel Level TraceMsg
    customLogLevel = CustomLogLevel $ \case
      TraceResolveHeader ResolveHeaderFound{}    -> const Debug
      TraceResolveHeader ResolveHeaderNotFound{} -> const Debug
      _otherTrace                                -> id

    -- | Check the @#include@ arguments, emitting trace messages
    checkInputs ::
        Tracer TraceMsg
      -> [C.UncheckedHashIncludeArg]
      -> IO [C.HashIncludeArg]
    checkInputs tracer = mapM $
        C.hashIncludeArgWithTrace
          (contramap (TraceBoot . BootHashIncludeArg) tracer)
