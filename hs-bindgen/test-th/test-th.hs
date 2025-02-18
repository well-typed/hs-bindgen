{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import HsBindgen.Runtime.ConstantArray qualified as CA
import Foreign (Storable (..), Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CLong)

import Test01

main :: IO ()
main = defaultMain $ testGroup CURRENT_COMPONENT_ID
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

    , testCase "function" $ do
        res <- my_fma 2 3 5
        res @?= 11
    ]

-- testing usage, e.g. that instances are present.

val :: MyStruct
val = MyStruct
    { myStruct_field1 = 0
    , myStruct_field2 = 1
    }

_pokeVal :: Ptr MyStruct -> IO ()
_pokeVal ptr = poke ptr val

_myPlus :: CLong -> CLong -> CLong
_myPlus x y = pLUS x y
