module Main (main) where

import Test.Tasty (defaultMain)

import Test.HsBindgen.HighLevel qualified as HighLevel

main :: IO ()
main = defaultMain HighLevel.tests
