module Main (main) where

import HsBindgen.App.Common
import HsBindgen.App.Dev

import HsBindgen.Lib

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = execDev =<< getDev

execDev :: Dev -> IO ()
execDev Dev{..} = case devCmd of
    DevCmdParse cmdOpts -> execParse devGlobalOpts cmdOpts

execParse :: GlobalOpts -> ParseOpts -> IO ()
execParse GlobalOpts{..} ParseOpts{..} = do
    let artefacts = ReifiedC :* Nil
    (I decls :* Nil) <- hsBindgen tracerConfig moduleUnique config bindingSpecConfig inputs artefacts
    print decls
  where
    moduleUnique = getModuleUnique config.configHsModuleOpts
