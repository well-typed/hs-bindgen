module Main (main) where

import Control.Tracer (Tracer)
import GHC.Stack (HasCallStack)

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
    withTracerStdOut (globalOptsTracerConf devGlobalOpts) $ \tracer ->
      execMode dev tracer devMode

execMode :: HasCallStack
  => Dev -> Tracer IO (TraceWithCallStack Trace) -> Mode -> IO ()
execMode Dev{..} tracer = \case
    ModeParse{..} -> do
      extBindings <- loadExtBindings' resolveHeaderTracer devGlobalOpts
      let opts = cmdOpts {
              optsExtBindings = extBindings
            }
      print . snd =<< Pipeline.parseCHeader opts parseInputPath
  where
    resolveHeaderTracer :: Tracer IO (TraceWithCallStack ResolveHeaderException)
    resolveHeaderTracer = useTrace TraceResolveHeader tracer

    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = globalOptsClangArgs devGlobalOpts
      , optsPredicate  = globalOptsPredicate devGlobalOpts
      , optsTracer = tracer
      }
