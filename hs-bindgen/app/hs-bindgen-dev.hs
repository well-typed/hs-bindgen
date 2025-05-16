module Main (main) where

import Control.Tracer (Contravariant (contramap), Tracer)

import HsBindgen.App.Common
import HsBindgen.App.Dev
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    dev@Dev{..} <- getDev
    withTracerStdOut (globalOptsVerbosity devGlobalOpts) $ \tracer ->
      execMode dev tracer devMode

execMode :: Dev -> Tracer IO Trace -> Mode -> IO ()
execMode Dev{..} tracer = \case
    ModeParse{..} -> do
      extBindings <- loadExtBindings' resolveHeaderTracer devGlobalOpts
      let opts = cmdOpts {
              optsExtBindings = extBindings
            }
      print . snd =<< Pipeline.parseCHeader opts parseInputPath
  where
    resolveHeaderTracer :: Tracer IO ResolveHeaderException
    resolveHeaderTracer = contramap TraceResolveHeader tracer

    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = globalOptsClangArgs devGlobalOpts
      , optsPredicate  = globalOptsPredicate devGlobalOpts
      , optsTracer = tracer
      }
