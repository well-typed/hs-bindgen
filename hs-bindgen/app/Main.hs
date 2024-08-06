module Main where

import HsBindgen.Cmdline
import HsBindgen.Preprocessor (generateModule)
import HsBindgen.Preprocessor.Render (render)
import HsBindgen.Spec qualified as Spec

main :: IO ()
main = do
    cmdline <- getCmdline
    spec'   <- Spec.resolve (cmdInput cmdline)
    writeFile (cmdOutput cmdline) $
      render (cmdRenderOptions cmdline) $
        generateModule spec'
