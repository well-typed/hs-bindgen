module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import HsBindgen.TestRuntime.GenSeq.Test qualified
import HsBindgen.TestRuntime.Preturb.Test qualified
import HsBindgen.TestRuntime.SameSemantics.Test qualified
import HsBindgen.TestRuntime.Storable.Test qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test-hs-bindgen-test-runtime"
    [ HsBindgen.TestRuntime.GenSeq.Test.tests
    , HsBindgen.TestRuntime.Preturb.Test.tests
    , HsBindgen.TestRuntime.SameSemantics.Test.tests
    , HsBindgen.TestRuntime.Storable.Test.tests
    ]
