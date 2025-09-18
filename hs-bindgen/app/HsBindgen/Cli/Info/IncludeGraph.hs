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
import HsBindgen.Frontend.Predicate qualified as Predicate
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
    , predicate       :: ParsePredicate
    , output          :: Maybe FilePath
    , inputs          :: [UncheckedHashIncludeArg]
    }

parseOpts :: Parser Opts
parseOpts =
    Opts
      <$> parseClangArgsConfig
      <*> parseParsePredicate
      <*> optional parseOutput'
      <*> parseInputs

parseOutput' :: Parser FilePath
parseOutput' = strOption $ mconcat [
      short 'o'
    , long "output"
    , metavar "PATH"
    , help "Output path for the graph"
    ]

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
    (includeGraph, isMainHeader, isInMainHeaderDir, _) <-
      exitOnClangError <=< withClang clangTracer clangSetup $
        fmap Just . processIncludes
    let p path =
             Predicate.matchParse isMainHeader isInMainHeaderDir path predicate
          && path /= RootHeader.name
    case output of
      Just path -> writeFile path $ IncludeGraph.dumpMermaid p includeGraph
      Nothing   -> putStr         $ IncludeGraph.dumpMermaid p includeGraph
  where
    hiaTracer :: Tracer IO HashIncludeArgMsg
    hiaTracer = contramap (TraceBoot . BootHashIncludeArg) tracer

    clangTracer :: Tracer IO ClangMsg
    clangTracer = contramap (TraceFrontend . FrontendClang) tracer
