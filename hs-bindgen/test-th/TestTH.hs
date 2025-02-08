module Main (main) where

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import HsBindgen.Runtime.ConstantArray qualified as CA
import Foreign (Storable (..))
import Foreign.Marshal.Alloc (alloca)

import HsBindgen.TestTH.Spliced


main :: IO ()
main = defaultMain $ testGroup "test-th"
    [ testCase "constants" $ do
        sizeOf (undefined :: MyStruct) @?= 8
        alignment (undefined :: MyStruct) @?= 4

        sizeOf (undefined :: Struct2) @?= 12
        alignment (undefined :: Struct2)  @?= 4

    , testCase "ConstantArray peek-poke-roundtrip" $ do
        let s = Struct2 5 (CA.repeat 12)
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr

        s' @?= s

    , testCase "Bitfield" $ do
        let s = Struct3 5 1 1 2
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr
        s' @?= s
    ]
