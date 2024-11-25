module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import HsBindgen.TestLib.Preturb.Test qualified
import HsBindgen.TestLib.SameSemantics.Test qualified
import HsBindgen.TestLib.Storable.Test qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test-hs-bindgen-testlib"
    [ HsBindgen.TestLib.Preturb.Test.tests
    , HsBindgen.TestLib.SameSemantics.Test.tests
    , HsBindgen.TestLib.Storable.Test.tests
    ]
