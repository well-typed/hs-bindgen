module Main (main) where

import GHC.Stack (HasCallStack)

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

execMode :: HasCallStack
  => Dev -> Mode -> IO ()
execMode Dev{..} = \case
    ModeParse{..} -> genBindings parseInputPaths >>= print
  where
    genBindings :: [CHeaderIncludePath] -> IO TranslationUnit
    genBindings inputPaths =
      withTracerStdOut (globalOptsTracerConf devGlobalOpts) DefaultLogLevel $
        \tracer -> do
          extBindings <- loadExtBindings tracer
                           (globalOptsClangArgs devGlobalOpts)
                           (globalOptsStdlibSpecs devGlobalOpts)
                           (globalOptsExtBindings devGlobalOpts)
          let opts :: Opts
              opts = def {
                  optsClangArgs      = globalOptsClangArgs devGlobalOpts
                , optsExtBindings    = extBindings
                , optsPredicate      = globalOptsPredicate devGlobalOpts
                , optsProgramSlicing = globalOptsProgramSlicing devGlobalOpts
                , optsTracer         = tracer
                }
          Pipeline.parseCHeaders opts inputPaths
