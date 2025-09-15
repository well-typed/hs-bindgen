module Main (main) where

import Test.HsBindgen.Runtime.Bitfield qualified
import Test.HsBindgen.Runtime.CEnum qualified
import Test.Tasty (defaultMain, testGroup)

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "test-runtime" [
      Test.HsBindgen.Runtime.Bitfield.tests
    , Test.HsBindgen.Runtime.CEnum.tests
    ]
