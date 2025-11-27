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
import HsBindgen.Config.ClangArgs
import HsBindgen.Frontend.RootHeader
import HsBindgen.Imports
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
    , inputs          :: [UncheckedHashIncludeArg]
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
exec GlobalOpts{..} Opts{..} = do
    eErr <- withTracer tracerConfig' $ \tracer _ -> do
      hashIncludeArgs <- checkInputs tracer inputs
      (clangArgs, _target) <-
        getClangArgsAndTarget (contramap TraceBoot tracer) clangArgsConfig
      includes <-
        resolveHeaders
          (contramap TraceResolveHeader tracer)
          clangArgs
          (Set.fromList hashIncludeArgs)
      fmap or . forM hashIncludeArgs $ \header' ->
        case Map.lookup header' includes of
          Just path -> (False <$) . putStrLn $
            "#include <" ++ getHashIncludeArg header'
              ++ "> resolved to " ++ show path
          Nothing   -> (True  <$) . putStrLn $
            "#include <" ++ getHashIncludeArg header'
              ++ "> could not be resolved (header not found)"
    case eErr of
      Right False -> return ()
      Right True  -> throwIO (ExitFailure 1)
      Left e      -> throwIO e
  where
    tracerConfig' :: TracerConfig Level TraceMsg
    tracerConfig' = tracerConfig{
        tCustomLogLevel = customLogLevel <> tCustomLogLevel tracerConfig
      }

    customLogLevel :: CustomLogLevel Level TraceMsg
    customLogLevel = CustomLogLevel $ \case
      TraceResolveHeader ResolveHeaderFound{}    -> const Debug
      TraceResolveHeader ResolveHeaderNotFound{} -> const Debug
      _otherTrace                                -> id

    -- | Check the @#include@ arguments, emitting trace messages
    checkInputs ::
        Tracer TraceMsg
      -> [UncheckedHashIncludeArg]
      -> IO [HashIncludeArg]
    checkInputs tracer = mapM $
        hashIncludeArgWithTrace
          (contramap (TraceBoot . BootHashIncludeArg) tracer)
