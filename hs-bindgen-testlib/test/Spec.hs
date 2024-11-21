module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import HsBindgen.TestLib.SameSemantics.Test qualified
import HsBindgen.TestLib.Storable.Test qualified
import HsBindgen.TestLib.Transform.Test qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test-hs-bindgen-testlib"
    [ HsBindgen.TestLib.SameSemantics.Test.tests
    , HsBindgen.TestLib.Storable.Test.tests
    , HsBindgen.TestLib.Transform.Test.tests
    ]
