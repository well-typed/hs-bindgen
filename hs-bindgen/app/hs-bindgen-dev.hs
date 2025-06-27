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
main = execDev =<< getDev

execDev :: Dev -> IO ()
execDev Dev{..} = case devMode of
    DevModeParse mode -> execParse devGlobalOpts mode

execParse :: GlobalOpts -> ParseMode -> IO ()
execParse globalOpts ParseMode{..} = print =<< doParse
  where
    doParse :: IO TranslationUnit
    doParse = withTracer globalOpts $ \tracer -> do
      extBindingSpec <- loadExtBindingSpecs' tracer globalOpts
      let opts = (getOpts globalOpts) {
              optsExtBindingSpec = extBindingSpec
            , optsTracer         = tracer
            }
      Pipeline.parseCHeaders opts parseInputPaths
