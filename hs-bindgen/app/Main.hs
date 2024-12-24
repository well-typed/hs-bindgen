{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Directory qualified as Dir
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

    relPath <- Just <$> Dir.getCurrentDirectory

    execMode relPath cmdline tracer (cmdMode)

execMode ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Cmdline
  -> Tracer IO String
  -> Mode
  -> IO ()
execMode relPath cmdline tracer = \case
    ModePreprocess{..} -> do
      cHeader <- parseC relPath cmdline tracer preprocessInput
      let hsModl = genModule preprocessModuleOpts cHeader
      prettyHs preprocessRenderOpts preprocessOutput hsModl
    ModeGenTests{..} -> do
      cHeader <- parseC relPath cmdline tracer genTestsInput
      genTests genTestsInput cHeader genTestsModuleOpts genTestsRenderOpts genTestsOutput
    Dev devMode ->
      execDevMode relPath cmdline tracer devMode

execDevMode ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Cmdline
  -> Tracer IO String
  -> DevMode
  -> IO ()
execDevMode relPath cmdline tracer = \case
    DevModeParseCHeader{..} ->
      prettyC =<< parseC relPath cmdline tracer parseCHeaderInput
    DevModePrelude{..} ->
      IO.withFile preludeLogPath IO.WriteMode $ \logHandle -> do
        _entries <- withC relPath cmdline tracer preludeInput $
          bootstrapPrelude relPath tracer (preludeLogTracer logHandle)
        return ()
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
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Cmdline
  -> Tracer IO String
  -> FilePath
  -> (CXTranslationUnit -> IO r)
  -> IO r
withC relPath cmdline tracer fp =
    withTranslationUnit relPath traceWarnings (cmdClangArgs cmdline) fp
  where
    traceWarnings :: Tracer IO Diagnostic
    traceWarnings = contramap show tracer

parseC ::
     Maybe FilePath -- ^ Directory to make paths relative to
  -> Cmdline
  -> Tracer IO String
  -> FilePath
  -> IO CHeader
parseC relPath cmdline tracer fp =
    withC relPath cmdline tracer fp $
      parseCHeader relPath traceSkipped (cmdPredicate cmdline)
  where
    traceSkipped :: Tracer IO Skipped
    traceSkipped = (contramap prettyLogMsg tracer)
