{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Unions (
    tests
  ) where

import Data.Primitive.ByteArray qualified as P
import Data.Word (Word64, Word8)
import Foreign.Marshal (with)
import Foreign.Storable (Storable (peek, sizeOf))
import GHC.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.Overloading

import Generated.Unions qualified as Types
import Generated.Unions.Unsafe qualified as Unsafe
import Test.Util.QC (byteArrayOf, shrinkByteArray)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Unions" [
      testProperty "prop_setGetX_Hs" prop_setGetX_Hs
    , testProperty "prop_setGetX_C"  prop_setGetX_C
    , testProperty "prop_getSetX_Hs" prop_getSetX_Hs
    , testProperty "prop_getSetX_C"  prop_getSetX_C
    , testProperty "prop_setGetY_Hs" prop_setGetY_Hs
    , testProperty "prop_setGetY_C"  prop_setGetY_C
    , testProperty "prop_getSetY_Hs" prop_getSetY_Hs
    , testProperty "prop_getSetY_C"  prop_getSetY_C
    ]

{-------------------------------------------------------------------------------
  setUnionPayload should not zero out unrelated bytes
-------------------------------------------------------------------------------}

-- See <https://github.com/well-typed/hs-bindgen/issues/2183>

-- x

-- | Roundtrip in Haskell: get after set should return the set value
prop_setGetX_Hs :: Types.U -> Large Word64 -> Property
prop_setGetX_Hs u (Large x) = x === (u { u_x = x }).u_x

-- | Roundtrip in C: get after set should return the set value
prop_setGetX_C :: Types.U -> Large Word64 -> Property
prop_setGetX_C u (Large x) = ioProperty $ do
    with u $ \ptr -> do
      Unsafe.set_u_x ptr x
      x' <- Unsafe.get_u_x ptr
      pure $ x === x'

-- | Roundtrip in Haskell: set after get should return the byte array to its
-- original state
prop_getSetX_Hs :: Types.U -> Property
prop_getSetX_Hs u = u === u { u_x = u.u_x }

-- | Roundtrip in C: set after get should return the union to its original state
prop_getSetX_C :: Types.U -> Property
prop_getSetX_C u = ioProperty $
    with u $ \ptr ->
    with u $ \ptr' -> do
      y <- Unsafe.get_u_x ptr'
      Unsafe.set_u_x ptr' y
      eqx <- Unsafe.eq_u_x ptr ptr'
      eqy <- Unsafe.eq_u_y ptr ptr'
      u' <- peek ptr'
      pure (1 === eqx .&&. 1 === eqy .&&. removeUnusedMem u === u')


-- y

-- | Roundtrip in Haskell: get after set should return the set value
prop_setGetY_Hs :: Types.U -> Large Word8 -> Property
prop_setGetY_Hs u (Large y) = y === (u { u_y = y }).u_y

-- | Roundtrip in C: get after set should return the set value
prop_setGetY_C :: Types.U -> Large Word8 -> Property
prop_setGetY_C u (Large y) = ioProperty $ do
    with u $ \ptr -> do
      Unsafe.set_u_y ptr y
      y' <- Unsafe.get_u_y ptr
      pure $ y === y'

-- | Roundtrip in Haskell: set after get should return the byte array to its
-- original state
prop_getSetY_Hs :: Types.U -> Property
prop_getSetY_Hs u = u === u { u_y = u.u_y }

-- | Roundtrip in C: set after get should return the union to its original state
prop_getSetY_C :: Types.U -> Property
prop_getSetY_C u = ioProperty $
    with u $ \ptr ->
    with u $ \ptr' -> do
      y <- Unsafe.get_u_y ptr'
      Unsafe.set_u_y ptr' y
      eqx <- Unsafe.eq_u_x ptr ptr'
      eqy <- Unsafe.eq_u_y ptr ptr'
      u' <- peek ptr'
      pure (1 === eqx .&&. 1 === eqy .&&. removeUnusedMem u === u')

{-------------------------------------------------------------------------------
  Orphans
-------------------------------------------------------------------------------}

deriving newtype instance Show Types.U
deriving newtype instance Eq Types.U

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

instance Arbitrary Types.U where
  arbitrary = Types.U <$> genBytes
    where
      uSz = sizeOf (undefined :: Types.U)

      genBytes = sized $ \n -> do
        k <- chooseInt (uSz, max uSz n)
        let genByte = getLarge <$> arbitrary
        byteArrayOf k genByte

  shrink x = fmap Types.U $ shrinkBytes x.unwrapU
    where
      uSz = sizeOf (undefined :: Types.U)

      shrinkBytes bytes = [
            bytes'
          | let shrinkByte = fmap getLarge . shrink . Large
          , bytes' <- shrinkByteArray shrinkByte bytes
          , uSz <= P.sizeofByteArray bytes'
          ]

{-# NOINLINE removeUnusedMem #-}
-- | Remove trailing bytes from the inner byte array
removeUnusedMem :: Types.U -> Types.U
removeUnusedMem u = unsafePerformIO $ with u peek
