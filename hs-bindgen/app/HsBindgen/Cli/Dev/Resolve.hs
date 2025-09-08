-- | @hs-bindgen-cli dev resolve@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Dev.Resolve qualified as Resolve
module HsBindgen.Cli.Dev.Resolve (
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
import System.Exit (ExitCode (ExitFailure))

import Options.Applicative hiding (info)

import HsBindgen.Boot (getClangArgs)
import HsBindgen.Imports
import HsBindgen.Lib

import HsBindgen.App

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Resolve C headers to source paths"

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data Opts = Opts {
      clangArgsConfig :: ClangArgsConfig
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
    eErr <- withTracer tracerConfig' $ \tracer -> do
      hashIncludeArgs <- checkInputs tracer inputs
      clangArgs <- getClangArgs (contramap TraceBoot tracer) clangArgsConfig
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
    tracerConfig' :: TracerConfig IO Level TraceMsg
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
        Tracer IO TraceMsg
      -> [UncheckedHashIncludeArg]
      -> IO [HashIncludeArg]
    checkInputs tracer = mapM $
        hashIncludeArgWithTrace
          (contramap (TraceBoot . BootHashIncludeArg) tracer)
