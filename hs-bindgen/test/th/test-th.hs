{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- For flexible array members:
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Exception (AssertionFailed (AssertionFailed),
                          Exception (fromException), TypeError (TypeError),
                          bracket, evaluate, try, tryJust)
import Control.Monad (unless, when)
import Data.Data (Proxy (Proxy))
import Data.Either (isRight)
import Data.Vector.Storable qualified as VS
import Foreign (Ptr, Storable (..), nullPtr, with)
import Foreign.C.Types (CInt, CLong)
import Foreign.Marshal.Alloc (alloca)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, HasCallStack, assertFailure, testCase,
                         (@?=))

import HsBindgen.Runtime.CEnum qualified as CEnum
import HsBindgen.Runtime.ConstantArray qualified as CA
import HsBindgen.Runtime.FlexibleArrayMember (FLAMLengthMismatch (FLAMLengthMismatch))
import HsBindgen.Runtime.FlexibleArrayMember qualified as FLAM
import HsBindgen.Runtime.LibC qualified as LibC
import HsBindgen.Runtime.CAPI (allocaAndPeek)

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

-- Bounded and Enum orphan instances
deriving via CEnum.AsCEnum Test01.EnumBasic  instance Bounded Test01.EnumBasic
deriving via CEnum.AsCEnum Test01.EnumBasic  instance Enum    Test01.EnumBasic
deriving via CEnum.AsCEnum Test01.EnumNeg    instance Bounded Test01.EnumNeg
deriving via CEnum.AsCEnum Test01.EnumNeg    instance Enum    Test01.EnumNeg
deriving via CEnum.AsCEnum Test01.EnumNonSeq instance Bounded Test01.EnumNonSeq
deriving via CEnum.AsCEnum Test01.EnumNonSeq instance Enum    Test01.EnumNonSeq
deriving via CEnum.AsCEnum Test01.EnumSame   instance Bounded Test01.EnumSame
deriving via CEnum.AsCEnum Test01.EnumSame   instance Enum    Test01.EnumSame

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

            -- Peek.
            structWithFLAM <- FLAM.peekWithFLAM ptr
            FLAM.flamExtra structWithFLAM @?= VS.fromList [0..9]

            -- Poke, Ok.
            let v' = VS.fromList [10..19]
            FLAM.pokeWithFLAM ptr (FLAM.WithFlexibleArrayMember hdr v')
            struct' <- FLAM.peekWithFLAM ptr
            FLAM.flamExtra struct' @?= v'

            -- Poke, error.
            let vLengthMismatch = VS.fromList [0]
            assertException "Expected FLAMLengthMismatch" (Proxy :: Proxy FLAMLengthMismatch) $
              FLAM.pokeWithFLAM ptr (FLAM.WithFlexibleArrayMember hdr vLengthMismatch)
            struct'' <- FLAM.peekWithFLAM ptr
            FLAM.flamExtra struct'' @?= v'

    , testCase "EnumBasic" $
        [minBound..maxBound]
          @?= [Test01.ENUM_BASIC_A, Test01.ENUM_BASIC_B, Test01.ENUM_BASIC_C]

    , testCase "EnumNeg" $
        [minBound..maxBound]
          @?= [Test01.ENUM_NEG_A, Test01.ENUM_NEG_B, Test01.ENUM_NEG_C]

    , testCase "EnumNonSeq" $
        [minBound..maxBound]
          @?= [ Test01.ENUM_NON_SEQ_A
              , Test01.ENUM_NON_SEQ_B
              , Test01.ENUM_NON_SEQ_C
              ]

    , testCase "EnumSame" $ do
        Test01.ENUM_SAME_B @?= Test01.ENUM_SAME_C
        [minBound..maxBound]
          @?= [Test01.ENUM_SAME_A, Test01.ENUM_SAME_B, Test01.ENUM_SAME_D]

    , testCase "struct-arg" $ do
        res <- thing_fun_1_high (Test01.Thing 10)
        10 @?= res

    , testCase "struct-res" $ do
        res <- thing_fun_2_high 11
        Test01.Thing 11 @?= res

    , testCase "struct-arg-res" $ do
        res <- thing_fun_3_high (Test01.Thing 6)
        Test01.Thing 12 @?= res
    ]

thing_fun_1_high :: Test01.Thing -> IO CInt
thing_fun_1_high x =
    with x $ \x' ->
    Test01.thing_fun_1 x'

thing_fun_2_high :: CInt -> IO Test01.Thing
thing_fun_2_high x =
    allocaAndPeek $ \res ->
    Test01.thing_fun_2 x res

thing_fun_3_high :: Test01.Thing -> IO Test01.Thing
thing_fun_3_high x =
    with x $ \x' ->
    allocaAndPeek $ \res ->
    Test01.thing_fun_3 x' res

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

{-------------------------------------------------------------------------------
  Helpers
-------------------------------------------------------------------------------}

assertException :: forall e a. Exception e => String -> Proxy e -> IO a -> Assertion
assertException msg exception action = do
  result <- tryJust (\x -> fromException x :: Maybe e) action
  when (isRight result) $ assertFailure msg
