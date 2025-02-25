{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- For flexible array members:
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception (bracket)
import Test.Tasty (testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import Foreign (Storable (..), Ptr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CLong)
import Data.Vector.Storable qualified as VS

import Test01

main :: IO ()
main = defaultMain $ testGroup CURRENT_COMPONENT_ID
    [ testCase "constants" $ do
        sizeOf (undefined :: StructBasic) @?= 8
        alignment (undefined :: StructBasic) @?= 4

        sizeOf (undefined :: StructFixedSizeArray) @?= 12
        alignment (undefined :: StructFixedSizeArray)  @?= 4

    , testCase "ConstantArray peek-poke-roundtrip" $ do
        let s = StructFixedSizeArray 5 (CA.repeat 12)
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr

        s' @?= s

    , testCase "Bitfield" $ do
        let s = StructBitfield 5 1 1 2
        s' <- alloca $ \ptr -> do
            poke ptr s
            peek ptr
        s' @?= s

    , testCase "function" $ do
        res <- my_fma 2 3 5
        res @?= 11

    , testCase "flam" $ do
        let n = 10
        bracket (flam_init n) flam_deinit $ \ptr -> do
            hdr <- peek ptr
            structFLAM_length hdr @?= n

            struct <- FLAM.peekWithFLAM ptr
            FLAM.flamExtra struct @?= VS.fromList [0..9]
    ]

-- StructBacic
-----------------------------------------------------------------------

-- StructBasic is generated
val :: StructBasic
val = StructBasic
    { structBasic_field1 = 0
    , structBasic_field2 = 1
    }

-- .. and can be poked
_pokeVal :: Ptr StructBasic -> IO ()
_pokeVal ptr = poke ptr val

-- Macros
-----------------------------------------------------------------------

_myPlus :: CLong -> CLong -> CLong
_myPlus x y = pLUS x y

-- Flexible array member
-----------------------------------------------------------------------

-- orphan instance
instance FLAM.HasFlexibleArrayLength CLong StructFLAM where
    flexibleArrayMemberLength x = fromIntegral (structFLAM_length x)
