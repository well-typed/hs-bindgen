{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- For flexible array members:
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception (bracket)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import Foreign (Storable (..), Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CLong)
import Data.Vector.Storable qualified as VS

import Test01 qualified

{-------------------------------------------------------------------------------
  Test01
-------------------------------------------------------------------------------}

-- StructBasic is generated
val :: Test01.StructBasic
val = Test01.StructBasic
    { Test01.structBasic_field1 = 0
    , Test01.structBasic_field2 = 1
    }

-- .. and can be poked
_pokeVal :: Ptr Test01.StructBasic -> IO ()
_pokeVal ptr = poke ptr val

-- Macros
_myPlus :: CLong -> CLong -> CLong
_myPlus x y = Test01.pLUS x y

-- Flexible array member (orphan instance)
instance FLAM.HasFlexibleArrayLength CLong Test01.StructFLAM where
    flexibleArrayMemberLength x = fromIntegral (Test01.structFLAM_length x)

-- Unit tests
test01 :: TestTree
test01 = testGroup "test_01"
    [ testCase "constants" $ do
        sizeOf (undefined :: Test01.StructBasic) @?= 8
        alignment (undefined :: Test01.StructBasic) @?= 4

        sizeOf (undefined :: Test01.StructFixedSizeArray) @?= 12
        alignment (undefined :: Test01.StructFixedSizeArray)  @?= 4

    , testCase "ConstantArray peek-poke-roundtrip" $ do
        let s = Test01.StructFixedSizeArray 5 (CA.repeat 12)
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr

        s' @?= s

    , testCase "Bitfield" $ do
        let s = Test01.StructBitfield 5 1 1 2
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr
        s' @?= s

    , testCase "function" $ do
        res <- Test01.my_fma 2 3 5
        res @?= 11

    , testCase "flam" $ do
        let n = 10
        bracket (Test01.flam_init n) Test01.flam_deinit $ \ptr -> do
            hdr <- peek ptr
            Test01.structFLAM_length hdr @?= n

            struct <- FLAM.peekWithFLAM ptr
            FLAM.flamExtra struct @?= VS.fromList [0..9]
    ]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup CURRENT_COMPONENT_ID
    [ test01
    ]
