-- | @hs-bindgen-cli info include-graph@ command
--
-- Intended for qualified import.
--
-- > import HsBindgen.Cli.Info.IncludeGraph qualified as IncludeGraph
module HsBindgen.Cli.Info.IncludeGraph (
    -- * CLI help
    info
    -- * Options
  , Opts(..)
  , parseOpts
    -- * Execution
  , exec
  ) where

import Control.Monad ((<=<))
import Options.Applicative hiding (info)
import System.Exit (ExitCode (ExitFailure))

import Clang.Enum.Bitfield
import Clang.LowLevel.Core
import Clang.Paths

import HsBindgen.App
import HsBindgen.Boot qualified as Boot
import HsBindgen.Clang
import HsBindgen.Frontend.Analysis.IncludeGraph qualified as IncludeGraph
import HsBindgen.Frontend.ProcessIncludes (processIncludes)
import HsBindgen.Frontend.RootHeader qualified as RootHeader
import HsBindgen.Imports
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  CLI help
-------------------------------------------------------------------------------}

info :: InfoMod a
info = progDesc "Compute the include graph"

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
exec GlobalOpts{..} opts =
    either throwIO return =<< withTracer tracerConfig (execWithTracer opts)

execWithTracer :: Opts -> Tracer IO TraceMsg -> IO ()
execWithTracer Opts{..} tracer = do
    hashIncludeArgs <- mapM (hashIncludeArgWithTrace hiaTracer) inputs
    let rootHeader = RootHeader.fromMainFiles hashIncludeArgs
    clangArgs <- Boot.getClangArgs (contramap TraceBoot tracer) clangArgsConfig
    let clangInput =
          ClangInputMemory
            (getSourcePath RootHeader.name)
            (RootHeader.content rootHeader)
        clangSetup = (defaultClangSetup clangArgs clangInput) {
            clangFlags = bitfieldEnum [
                CXTranslationUnit_DetailedPreprocessingRecord
              ]
          }
        exitOnClangError = maybe (throwIO $ ExitFailure 1) return
    includeGraph <-
      exitOnClangError <=< withClang clangTracer clangSetup $ \unit -> do
        (includeGraph, _, _, _) <- processIncludes unit
        return $ Just includeGraph
    putStrLn $ IncludeGraph.dumpMermaid includeGraph
  where
    hiaTracer :: Tracer IO HashIncludeArgMsg
    hiaTracer = contramap (TraceBoot . BootHashIncludeArg) tracer

    clangTracer :: Tracer IO ClangMsg
    clangTracer = contramap (TraceFrontend . FrontendClang) tracer
