module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import Test.HsBindgen.Runtime.Bitfield qualified
import Test.HsBindgen.Runtime.CBool qualified
import Test.HsBindgen.Runtime.CEnum qualified
import Test.HsBindgen.Runtime.ConstantArray qualified
import Test.HsBindgen.Runtime.IncompleteArray qualified
import Test.HsBindgen.Runtime.SizedByteArray qualified

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup "test-runtime" [
      Test.HsBindgen.Runtime.Bitfield.tests
    , Test.HsBindgen.Runtime.CBool.tests
    , Test.HsBindgen.Runtime.CEnum.tests
    , Test.HsBindgen.Runtime.ConstantArray.tests
    , Test.HsBindgen.Runtime.IncompleteArray.tests
    , Test.HsBindgen.Runtime.SizedByteArray.tests
    ]
