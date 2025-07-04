module Main (main) where

import HsBindgen.App.Common
import HsBindgen.App.Dev

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
execParse globalOpts ParseOpts{..} =
  doParse >>= fromMaybeWithFatalError >>= print
  where
    doParse :: IO (Maybe TranslationUnit)
    doParse = withTracer globalOpts $ \tracer -> do
      extSpec <- loadExtBindingSpecs' tracer globalOpts
      pSpec   <- loadPrescriptiveBindingSpec' tracer globalOpts
      let config = getConfig globalOpts
      Pipeline.parseCHeaders
        tracer
        config
        extSpec
        pSpec
        parseInputPaths
