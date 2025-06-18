module Main (main) where

import Data.ByteString qualified as BS
import GHC.Stack (HasCallStack)

import HsBindgen.App.Common
import HsBindgen.App.Dev
import HsBindgen.BindingSpec qualified as BindingSpec
import HsBindgen.BindingSpec.Stdlib qualified as Stdlib
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
    ModeBindingSpec BindingSpecModeBase ->
      BS.putStr $ BindingSpec.encodeYaml Stdlib.baseBindings
    ModeBindingSpec BindingSpecModeRuntime ->
      BS.putStr $ BindingSpec.encodeYaml Stdlib.runtimeBindings
  where
    genBindings :: [CHeaderIncludePath] -> IO TranslationUnit
    genBindings inputPaths =
      withTracerStdOut (globalOptsTracerConf devGlobalOpts) DefaultLogLevel $
        \tracer -> do
          extBindings <- loadExtBindings' tracer devGlobalOpts
          let opts :: Opts
              opts = def {
                  optsClangArgs   = globalOptsClangArgs devGlobalOpts
                , optsExtBindings = extBindings
                , optsPredicate   = globalOptsPredicate devGlobalOpts
                , optsTracer      = tracer
                }
          Pipeline.parseCHeaders opts inputPaths
