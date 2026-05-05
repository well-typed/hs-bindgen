module Test.PointerManipulation.Unions.Bitfields (
    tests
  ) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Unions.Bitfields" [
    ]

-- TODO <https://github.com/well-typed/hs-bindgen/issues/1253>: implement these
-- tests once bit-fields in unions are properly supported
