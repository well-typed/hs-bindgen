module Main (main) where

import HsBindgen.App.Cmdline
import HsBindgen.Bootstrap.Prelude (genPrelude)
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
    ModePreprocess{input, moduleOpts, renderOpts, output} ->
      preprocess $ Preprocess {
          preprocessTraceWarnings = contramap show tracer
        , preprocessTraceSkipped  = contramap prettyLogMsg tracer
        , preprocessPredicate     = cmdPredicate
        , preprocessClangArgs     = cmdClangArgs
        , preprocessInputPath     = input
        , preprocessModuleOpts    = moduleOpts
        , preprocessRenderOpts    = renderOpts
        , preprocessOutputPath    = output
        }
    Dev devMode ->
      execDevMode cmdline tracer devMode
  where
    Cmdline{cmdPredicate, cmdClangArgs} = cmdline

execDevMode :: Cmdline -> Tracer IO String -> DevMode -> IO ()
execDevMode cmdline tracer = \case
    DevModeParseCHeader fp -> do
      cHeader <-
        parseCHeader
          (contramap show tracer)
          (contramap prettyLogMsg tracer)
          cmdPredicate
          cmdClangArgs
          fp
      prettyC cHeader
    DevModePrelude ->
      genPrelude (prettyLogMsg `contramap` tracer)
  where
    Cmdline{cmdPredicate, cmdClangArgs} = cmdline
