module Main (main) where

import HsBindgen.App.Common
import HsBindgen.App.Dev

import HsBindgen.Lib

-- TODO: Remove when 'hsBindgen' is exported by Lib.
import HsBindgen (hsBindgen)

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
    let artefacts = _
    _ <- hsBindgen tracerConfig moduleUnique config bindingSpecConfig inputs artefacts
  where
    moduleUnique = getModuleUnique config.configHsModuleOpts
