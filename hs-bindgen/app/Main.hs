module Main (main) where

import HsBindgen.App.Cmdline
import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    cmdline@Cmdline{cmdVerbosity, cmdMode} <- getCmdline

    let tracer :: Tracer IO String
        tracer = mkTracerIO cmdVerbosity

    execMode cmdline tracer (cmdMode)

execMode :: Cmdline -> Tracer IO String -> Mode -> IO ()
execMode cmdline tracer = \case
    ModePreprocess{input, moduleOpts, renderOpts, output} -> do
      cHeader <- parseC cmdline tracer input
      let hsModl = genModule moduleOpts cHeader
      prettyHs renderOpts output hsModl
    Dev devMode ->
      execDevMode cmdline tracer devMode

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode cmdline tracer = \case
    DevModeParseCHeader fp ->
      prettyC =<< parseC cmdline tracer fp
    DevModePrelude fp -> do
      _entries <- withC cmdline tracer fp $ bootstrapPrelude tracer
      return ()

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