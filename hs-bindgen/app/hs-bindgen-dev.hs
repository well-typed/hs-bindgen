module Main (main) where

import HsBindgen.App.Common
import HsBindgen.App.Dev
import HsBindgen.Frontend.AST.External (TranslationUnit)
import HsBindgen.Lib
import HsBindgen.Pipeline qualified as Pipeline

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    dev@Dev{..} <- getDev
    execMode dev devMode

execMode :: Dev -> Mode -> IO ()
execMode Dev{..} = \case
    ModeParse{..} -> genBindings parseInputPaths >>= print
  where
    genBindings :: [CHeaderIncludePath] -> IO TranslationUnit
    genBindings inputPaths =
      withTracerStdOut (globalOptsTracerConf devGlobalOpts) DefaultLogLevel $
        \tracer -> do
          extBindingSpec <-
            loadExtBindingSpecs tracer
              (globalOptsClangArgs devGlobalOpts)
              (globalOptsStdlibSpecConf devGlobalOpts)
              (globalOptsExtBindings devGlobalOpts)
          let opts :: Opts
              opts = def {
                  optsClangArgs      = globalOptsClangArgs devGlobalOpts
                , optsExtBindingSpec = extBindingSpec
                , optsPredicate      = globalOptsPredicate devGlobalOpts
                , optsProgramSlicing = globalOptsProgramSlicing devGlobalOpts
                , optsTracer         = tracer
                }
          Pipeline.parseCHeaders opts inputPaths
