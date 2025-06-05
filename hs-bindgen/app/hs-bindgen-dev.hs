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
    _ <- withTracerStdOut (globalOptsTracerConf devGlobalOpts) DefaultLogLevel $ \tracer ->
      execMode dev tracer devMode
    pure ()

execMode :: HasCallStack
  => Dev -> Tracer IO (TraceWithCallStack Trace) -> Mode -> IO ()
execMode Dev{..} tracer = \case
    ModeParse{..} -> do
      extBindings <- loadExtBindings' tracer devGlobalOpts
      let opts = cmdOpts {
              optsExtBindings = extBindings
            }
      print =<< Pipeline.parseCHeader opts parseInputPath
  where
    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = globalOptsClangArgs devGlobalOpts
      , optsPredicate  = globalOptsPredicate devGlobalOpts
      , optsTracer = tracer
      }
