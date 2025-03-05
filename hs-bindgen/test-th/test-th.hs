{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- For flexible array members:
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception (bracket)
import Data.Vector.Storable qualified as VS
import Foreign (Storable (..), Ptr, nullPtr)
import Foreign.C.Types (CLong)
import Foreign.Marshal.Alloc (alloca)
import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (testCase, (@?=))

import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import HsBindgen.Runtime.LibC qualified as LibC

import Test01 qualified
import Test02 qualified

{-------------------------------------------------------------------------------
  Test01
-------------------------------------------------------------------------------}

-- StructBasic is generated
t01Val :: Test01.StructBasic
t01Val = Test01.StructBasic
    { Test01.structBasic_field1 = 0
    , Test01.structBasic_field2 = 1
    }

-- .. and can be poked
_t01PokeVal :: Ptr Test01.StructBasic -> IO ()
_t01PokeVal ptr = poke ptr t01Val

-- Macros
_t01MyPlus :: CLong -> CLong -> CLong
_t01MyPlus x y = Test01.pLUS x y

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
  Test02
-------------------------------------------------------------------------------}

-- Event is generated
t02Val :: Test02.Event
t02Val = Test02.Event
    { Test02.event_id   = 42
    , Test02.event_name = nullPtr
    , Test02.event_time = LibC.CTm
        { LibC.cTm_sec   = 5
        , LibC.cTm_min   = 4
        , LibC.cTm_hour  = 3
        , LibC.cTm_mday  = 2
        , LibC.cTm_mon   = 1
        , LibC.cTm_year  = 2000 - 1900
        , LibC.cTm_wday  = 6
        , LibC.cTm_yday  = 2
        , LibC.cTm_isdst = 0
        }
    }

-- Unit tests
test02 :: TestTree
test02 = testGroup "test_02"
    [ testCase "Event peek-poke-roundtrip" $ do
        x <- alloca $ \ptr -> do
            poke ptr t02Val
            peek ptr
        x @?= t02Val
    ]

{-------------------------------------------------------------------------------
  Main
-------------------------------------------------------------------------------}

main :: IO ()
main = defaultMain $ testGroup CURRENT_COMPONENT_ID
    [ test01
    , test02
    ]
