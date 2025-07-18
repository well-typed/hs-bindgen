module Main (main) where

import HsBindgen.App.Common
import HsBindgen.App.Dev

import HsBindgen.Lib

-- It is OK to import some internal libraries for our development client.
import HsBindgen.Frontend.AST.External (TranslationUnit)
import HsBindgen.Pipeline qualified as Pipeline

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = execDev =<< getDev

execDev :: Dev -> IO ()
execDev Dev{..} = case devCmd of
    DevCmdParse cmdOpts -> execParse devGlobalOpts cmdOpts

execParse :: GlobalOpts -> ParseOpts -> IO ()
execParse GlobalOpts{..} opts =
  doParse >>= fromMaybeWithFatalError >>= print
  where
    doParse :: IO (Maybe TranslationUnit)
    doParse = withTracerStdOut tracerConfig $ \tracer -> do
      (extSpec, pSpec) <- loadBindingSpecs
                            tracer
                            opts.config.configClangArgs
                            opts.bindingSpecConfig
      Pipeline.parseCHeaders
        tracer
        opts.config
        extSpec
        pSpec
        opts.inputPaths
