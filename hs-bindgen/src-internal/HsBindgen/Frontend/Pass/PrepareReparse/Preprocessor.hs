-- | Run the @clang@ executable's preprocessor
--
-- This module is intended to be imported unqualified. It is also intended to
-- only be imported from within the "HsBindgen.Frontend.Pass.PrepareReparse"
-- module hierarchy.
--
-- > import HsBindgen.Frontend.Pass.PrepareReparse.Preprocessor
--
module HsBindgen.Frontend.Pass.PrepareReparse.Preprocessor (
    preprocess
  ) where

import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode, showCommandForUser)

import Clang.Args (ClangArgs (unClangArgs))

import HsBindgen.Clang (ClangSetup (args))
import HsBindgen.Clang.Discover (ClangExe)
import HsBindgen.Frontend.Pass (IsPass (Msg))
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass (PrepareReparse)
import HsBindgen.Frontend.Pass.PrepareReparse.IsPass.Msg (PrepareReparseMsg (..))
import HsBindgen.Frontend.Pass.PrepareReparse.Tracer (traceImmediate)
import HsBindgen.Imports (Exception)
import HsBindgen.Util.Tracer (Tracer)

preprocess ::
     Tracer (Msg PrepareReparse)
  -> ClangExe
  -> ClangSetup
  -> FilePath
  -> IO (Either PreprocessorError String)
preprocess tr clangExe setup headerPath = do
    traceImmediate tr $ PrepareReparsePreprocessorCommand (showCommandForUser clangExe args)
    (ex, stdout, stderr) <- runClangExecutable clangExe args ""
    traceImmediate tr $ PrepareReparsePreprocessorExitCode ex
    traceImmediate tr $ PrepareReparsePreprocessorStdout stdout
    traceImmediate tr $ PrepareReparsePreprocessorStderr stderr
    case ex of
      ExitSuccess -> pure $ Right stdout
      ExitFailure n -> pure $ Left (PreprocessorError n stderr)
  where
    args = setup.args.unClangArgs ++ ["-E", "-C",  headerPath]

data PreprocessorError = PreprocessorError Int String
  deriving stock Show
  deriving anyclass Exception

runClangExecutable :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
runClangExecutable = readProcessWithExitCode
