{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad
import Text.Read (readMaybe)
import System.IO qualified as IO

import HsBindgen.App.Cmdline
import HsBindgen.Clang.Paths
import HsBindgen.ExtBindings
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
execMode cmdline@Cmdline{..} tracer = \case
    ModePreprocess{..} -> do
      includePathDirs <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePathDirs preprocessInput
      extBindings <- loadExtBindings' includePathDirs cmdExtBindings
      cHeader <- parseC cmdline tracer extBindings absPath
      let hsModl = genModule preprocessInput preprocessTranslationOpts preprocessModuleOpts cHeader
      prettyHs preprocessRenderOpts preprocessOutput hsModl
    ModeGenTests{..} -> do
      includePathDirs <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePathDirs genTestsInput
      extBindings <- loadExtBindings' includePathDirs cmdExtBindings
      cHeader <- parseC cmdline tracer extBindings absPath
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
execDevMode cmdline@Cmdline{..} tracer = \case
    DevModeParseCHeader{..} -> do
      includePathDirs <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePathDirs parseCHeaderInput
      extBindings <- loadExtBindings' includePathDirs cmdExtBindings
      prettyC =<< parseC cmdline tracer extBindings absPath
    DevModePrelude{..} -> do
      includePathDir <- either fail return <=< resolveCIncludeAbsPathDirs $
        clangSystemIncludePathDirs cmdClangArgs
          ++ clangIncludePathDirs cmdClangArgs
      absPath <- either fail return
        =<< resolveHeader includePathDir preludeInput
      IO.withFile preludeLogPath IO.WriteMode $ \logHandle -> do
        void . withC cmdline tracer absPath $
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

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

withC ::
     Cmdline
  -> Tracer IO String
  -> CHeaderAbsPath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC cmdline tracer headerPath =
    withTranslationUnit traceWarnings (cmdClangArgs cmdline) headerPath
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show tracer

parseC ::
     Cmdline
  -> Tracer IO String
  -> ExtBindings
  -> CHeaderAbsPath
  -> IO CHeader
parseC cmdline tracer extBindings headerPath = do
    withC cmdline tracer headerPath $
      parseCHeader traceSkipped (cmdPredicate cmdline) extBindings
  where
    traceSkipped :: Tracer IO Skipped
    traceSkipped = (contramap prettyLogMsg tracer)
