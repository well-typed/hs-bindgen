module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import HsBindgen.TestLib.Transform.Test qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "test-hs-bindgen-testlib"
    [ HsBindgen.TestLib.Transform.Test.tests
    ]
