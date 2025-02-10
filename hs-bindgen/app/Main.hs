{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (void)
import Text.Read (readMaybe)
import System.IO qualified as IO

import HsBindgen.App.Cmdline
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline@Cmdline{..} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline tracer = \case
    ModePreprocess{..} -> do
      cHeader <- parseC cmdline tracer preprocessInput
      let hsModl = genModule preprocessInput preprocessTranslationOpts preprocessModuleOpts cHeader
      prettyHs preprocessRenderOpts preprocessOutput hsModl
    ModeGenTests{..} -> do
      cHeader <- parseC cmdline tracer genTestsInput
      genTests genTestsInput cHeader genTestsModuleOpts genTestsRenderOpts genTestsOutput
    ModeLiterate input output -> do
      lit <- readFile input
      args <- maybe (fail "cannot parse literate file") return $ readMaybe lit
      mode <- maybe (fail "cannot parse arguments in literate file") return $ pureParseModePreprocess args
      execMode cmdline tracer $ case mode of
          ModePreprocess {} -> mode { preprocessOutput = Just output }
          _ -> mode

    Dev devMode ->
      execDevMode cmdline tracer devMode

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode cmdline tracer = \case
    DevModeParseCHeader{..} ->
      prettyC =<< parseC cmdline tracer parseCHeaderInput
    DevModePrelude{..} -> do
      let cmdline' = preludeCmdline preludeIncludeDir
      IO.withFile preludeLogPath IO.WriteMode $ \logHandle -> do
        void . withC cmdline' tracer preludeInput $
          bootstrapPrelude tracer (preludeLogTracer logHandle)
  where
    preludeLogPath :: FilePath
    preludeLogPath = "macros-recognized.log"

    preludeLogTracer :: IO.Handle -> Tracer IO String
    preludeLogTracer logHandle =
      mkTracer
        (IO.hPutStrLn logHandle . ("Error: "   ++))
        (IO.hPutStrLn logHandle . ("Warning: " ++))
        (IO.hPutStrLn logHandle)
        True

    preludeCmdline :: FilePath -> Cmdline
    preludeCmdline includeDir = cmdline {
        cmdClangArgs = preludeClangArgs includeDir $ cmdClangArgs cmdline
      }

    preludeClangArgs :: FilePath -> ClangArgs -> ClangArgs
    preludeClangArgs includeDir clangArgs = clangArgs {
        clangOtherArgs =
          preludeClangOtherArgs includeDir $ clangOtherArgs clangArgs
      }

    preludeClangOtherArgs :: FilePath -> [String] -> [String]
    preludeClangOtherArgs includeDir args =
        "-nostdinc" : "-isystem" : includeDir : args

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withC ::
     Cmdline
  -> Tracer IO String
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC cmdline tracer fp =
    withTranslationUnit traceWarnings (cmdClangArgs cmdline) fp
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show tracer

parseC ::
     Cmdline
  -> Tracer IO String
  -> FilePath
  -> IO CHeader
parseC cmdline tracer fp =
    withC cmdline tracer fp $
      parseCHeader traceSkipped (cmdPredicate cmdline)
  where
    traceSkipped :: Tracer IO Skipped
    traceSkipped = (contramap prettyLogMsg tracer)
