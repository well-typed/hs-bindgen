module Main (main) where

import HsBindgen.Cmdline
import HsBindgen.Lib

main :: IO ()
main = do
    cmdline <- getCmdline
    execSpec (cmdSpec cmdline)
