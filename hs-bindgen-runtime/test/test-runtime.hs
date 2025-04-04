module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.HsBindgen.Runtime.Bitfield qualified

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "test-runtime" [
      Test.HsBindgen.Runtime.Bitfield.tests
    ]
