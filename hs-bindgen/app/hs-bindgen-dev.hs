module Main (main) where

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

    let tracer :: Tracer IO String
        tracer = mkTracerIO $ globalOptsVerbosity devGlobalOpts

    execMode dev tracer devMode

execMode :: Dev -> Tracer IO String -> Mode -> IO ()
execMode Dev{..} tracer = \case
    ModeParse{..} -> do
      extBindingSpecs <- loadExtBindingSpecs' tracer devGlobalOpts
      let opts = cmdOpts {
              optsExtBindingSpecs = extBindingSpecs
            }
      print . snd =<< Pipeline.parseCHeader opts parseInputPath
  where
    cmdOpts :: Opts
    cmdOpts = defaultOpts {
        optsClangArgs  = globalOptsClangArgs devGlobalOpts
      , optsPredicate  = globalOptsPredicate devGlobalOpts
      , optsDiagTracer = tracer
      , optsSkipTracer = tracer
      }
