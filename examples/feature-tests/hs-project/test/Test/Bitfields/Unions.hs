{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | C bit-field tests for unions
--
-- This module implements two types of tests for various @union@s:
--
-- * Peek tests use a C function to set the fields of a @union@, peek the
--   @union@, and check that the read field values are as expected.
-- * Poke tests poke the @union@ and then use a C function to check that the
--   fields are as expected.
--
-- Much of the code in this module follows the same pattern. Some of it has been
-- abstracted using TH, but not all of it, because this whole module will
-- eventually be replaced with generated tests.
module Test.Bitfields.Unions (tests) where

import Data.Proxy (Proxy (Proxy))
import Foreign qualified
import Foreign.C qualified as C
import Foreign.C.Types
import Foreign.Storable (Storable)
import GHC.Exts (proxy#)
import GHC.Records (HasField (getField))
import GHC.TypeLits (KnownSymbol)
import Test.QuickCheck ((===))
import Test.QuickCheck qualified as QC
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

import HsBindgen.Runtime.HasCBitfield qualified as HasCBitfield
import HsBindgen.Runtime.Prelude
import HsBindgen.Runtime.Support.Bitfield (Bitfield)
import HsBindgen.Runtime.Support.CompatHasField qualified as Compat
import HsBindgen.Runtime.Union as Union

import Generated.Bitfields.Unions qualified as Bitfields
import Generated.Bitfields.Unions.Unsafe qualified as Bitfields
import Test.Bitfields.Structs (allocaAligned, initValue)
import Test.Bitfields.Unions.TH

{-------------------------------------------------------------------------------
  not packed, <=8-bit fields
-------------------------------------------------------------------------------}

newtype U_8 fn = U_8 Bitfields.U_8
  deriving newtype (StaticSize, Storable)

instance HasCBitfield Bitfields.U_8 fn => HasCBitfield (U_8 fn) fn where
  type CBitfieldType (U_8 fn) fn = CBitfieldType Bitfields.U_8 fn
  bitfieldOffset# _ = bitfieldOffset# (proxy# @Bitfields.U_8)
  bitfieldWidth# _  = bitfieldWidth# (proxy# @Bitfields.U_8)

instance (
      ty ~ CBitfieldType Bitfields.U_8 fn
    , HasField fn Bitfields.U_8 ty
    ) => HasField fn (U_8 fn) ty where
  getField (U_8 x) = getField @fn x

instance (
      HasCBitfield Bitfields.U_8 fn
    , Compat.HasField fn Bitfields.U_8 (CBitfieldType Bitfields.U_8 fn)
    , Bitfield (CBitfieldType Bitfields.U_8 fn)
    , QC.Arbitrary (CBitfieldType Bitfields.U_8 fn)
    ) => QC.Arbitrary (U_8 fn) where
  arbitrary = do
      w1 <- QC.arbitrarySizedBoundedIntegral
      pure $ U_8 $ Union.set @fn $ initValue off sz w1
    where
      off = HasCBitfield.offset (Proxy @Bitfields.U_8) (Proxy @fn)
      sz  = HasCBitfield.width  (Proxy @Bitfields.U_8) (Proxy @fn)

  shrink (U_8 x) = [
        U_8 $ Union.set @fn x'
      | x' <- QC.shrink (Union.get @fn x)
      ]

$(showD ''U_8)
$(eqD ''U_8)

$(hasSetFunD "u_8_a" ''U_8 ''CSChar 'Bitfields.set_U_8_a)
$(hasSetFunD "u_8_b" ''U_8 ''CSChar 'Bitfields.set_U_8_b)
$(hasSetFunD "u_8_c" ''U_8 ''CSChar 'Bitfields.set_U_8_c)
$(hasSetFunD "u_8_d" ''U_8 ''CSChar 'Bitfields.set_U_8_d)
$(hasSetFunD "u_8_e" ''U_8 ''CSChar 'Bitfields.set_U_8_e)
$(hasSetFunD "u_8_f" ''U_8 ''CSChar 'Bitfields.set_U_8_f)

$(hasGetFunD "u_8_a" ''U_8 ''CSChar 'Bitfields.get_U_8_a)
$(hasGetFunD "u_8_b" ''U_8 ''CSChar 'Bitfields.get_U_8_b)
$(hasGetFunD "u_8_c" ''U_8 ''CSChar 'Bitfields.get_U_8_c)
$(hasGetFunD "u_8_d" ''U_8 ''CSChar 'Bitfields.get_U_8_d)
$(hasGetFunD "u_8_e" ''U_8 ''CSChar 'Bitfields.get_U_8_e)
$(hasGetFunD "u_8_f" ''U_8 ''CSChar 'Bitfields.get_U_8_f)

$(hasEqFunD "u_8_a" ''U_8 ''CSChar 'Bitfields.eq_U_8_a)
$(hasEqFunD "u_8_b" ''U_8 ''CSChar 'Bitfields.eq_U_8_b)
$(hasEqFunD "u_8_c" ''U_8 ''CSChar 'Bitfields.eq_U_8_c)
$(hasEqFunD "u_8_d" ''U_8 ''CSChar 'Bitfields.eq_U_8_d)
$(hasEqFunD "u_8_e" ''U_8 ''CSChar 'Bitfields.eq_U_8_e)
$(hasEqFunD "u_8_f" ''U_8 ''CSChar 'Bitfields.eq_U_8_f)

test_U_8 :: TestTree
test_U_8 = testGroup "<=8-bit fields" [
      -- peek
      testProperty "peek @u_8_a" $ peek_prop @"u_8_a"
    , testProperty "peek @u_8_b" $ peek_prop @"u_8_b"
    , testProperty "peek @u_8_c" $ peek_prop @"u_8_c"
    , testProperty "peek @u_8_d" $ peek_prop @"u_8_d"
    , testProperty "peek @u_8_e" $ peek_prop @"u_8_e"
    , testProperty "peek @u_8_f" $ peek_prop @"u_8_f"

      -- poke
    , testProperty "poke @u_8_a" $ poke_prop @"u_8_a"
    , testProperty "poke @u_8_b" $ poke_prop @"u_8_b"
    , testProperty "poke @u_8_c" $ poke_prop @"u_8_c"
    , testProperty "poke @u_8_d" $ poke_prop @"u_8_d"
    , testProperty "poke @u_8_e" $ poke_prop @"u_8_e"
    , testProperty "poke @u_8_f" $ poke_prop @"u_8_f"
    ]
  where
    peek_prop ::
        forall fn. (
           U_8_C fn
         , HasSetFun (U_8 fn) (CBitfieldType Bitfields.U_8 fn)
         )
      => U_8 fn -> QC.Property
    peek_prop x = QC.ioProperty $ do
        y <- allocaAligned $ \ptr -> do
          setFun @(U_8 fn) @(CBitfieldType Bitfields.U_8 fn)
            ptr
            (getField @fn x)
          Foreign.peek ptr
        return $ y === x

    poke_prop ::
         forall fn. (
           U_8_C fn
         , HasEqFun (U_8 fn) (CBitfieldType Bitfields.U_8 fn)
         )
      => U_8 fn -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        eqFun @(U_8 fn) @(CBitfieldType Bitfields.U_8 fn)
          ptr
          (getField @fn x)
      return $ isEq === C.CBool 1

type U_8_C fn = (
      HasField fn Bitfields.U_8 (CBitfieldType Bitfields.U_8 fn)
    , Eq (CBitfieldType Bitfields.U_8 fn)
    , Show (CBitfieldType Bitfields.U_8 fn)
    , KnownSymbol fn
    )

{-------------------------------------------------------------------------------
  not packed, <=16-bit fields
-------------------------------------------------------------------------------}

newtype U_16 fn = U_16 Bitfields.U_16
  deriving newtype (StaticSize, Storable)

instance HasCBitfield Bitfields.U_16 fn => HasCBitfield (U_16 fn) fn where
  type CBitfieldType (U_16 fn) fn = CBitfieldType Bitfields.U_16 fn
  bitfieldOffset# _ = bitfieldOffset# (proxy# @Bitfields.U_16)
  bitfieldWidth# _  = bitfieldWidth# (proxy# @Bitfields.U_16)

instance (
      ty ~ CBitfieldType Bitfields.U_16 fn
    , HasField fn Bitfields.U_16 ty
    ) => HasField fn (U_16 fn) ty where
  getField (U_16 x) = getField @fn x

instance (
      HasCBitfield Bitfields.U_16 fn
    , Compat.HasField fn Bitfields.U_16 (CBitfieldType Bitfields.U_16 fn)
    , Bitfield (CBitfieldType Bitfields.U_16 fn)
    , QC.Arbitrary (CBitfieldType Bitfields.U_16 fn)
    ) => QC.Arbitrary (U_16 fn) where
  arbitrary = do
      w1 <- QC.arbitrarySizedBoundedIntegral
      pure $ U_16 $ Union.set @fn $ initValue off sz w1
    where
      off = HasCBitfield.offset (Proxy @Bitfields.U_16) (Proxy @fn)
      sz  = HasCBitfield.width  (Proxy @Bitfields.U_16) (Proxy @fn)

  shrink (U_16 x) = [
        U_16 $ Union.set @fn x'
      | x' <- QC.shrink (Union.get @fn x)
      ]

$(showD ''U_16)
$(eqD ''U_16)

$(hasSetFunD "u_16_a" ''U_16 ''CSChar 'Bitfields.set_U_16_a)
$(hasSetFunD "u_16_b" ''U_16 ''CInt   'Bitfields.set_U_16_b)
$(hasSetFunD "u_16_c" ''U_16 ''CInt   'Bitfields.set_U_16_c)
$(hasSetFunD "u_16_d" ''U_16 ''CInt   'Bitfields.set_U_16_d)
$(hasSetFunD "u_16_e" ''U_16 ''CInt   'Bitfields.set_U_16_e)
$(hasSetFunD "u_16_f" ''U_16 ''CInt   'Bitfields.set_U_16_f)

$(hasGetFunD "u_16_a" ''U_16 ''CSChar 'Bitfields.get_U_16_a)
$(hasGetFunD "u_16_b" ''U_16 ''CInt   'Bitfields.get_U_16_b)
$(hasGetFunD "u_16_c" ''U_16 ''CInt   'Bitfields.get_U_16_c)
$(hasGetFunD "u_16_d" ''U_16 ''CInt   'Bitfields.get_U_16_d)
$(hasGetFunD "u_16_e" ''U_16 ''CInt   'Bitfields.get_U_16_e)
$(hasGetFunD "u_16_f" ''U_16 ''CInt   'Bitfields.get_U_16_f)

$(hasEqFunD "u_16_a" ''U_16 ''CSChar 'Bitfields.eq_U_16_a)
$(hasEqFunD "u_16_b" ''U_16 ''CInt   'Bitfields.eq_U_16_b)
$(hasEqFunD "u_16_c" ''U_16 ''CInt   'Bitfields.eq_U_16_c)
$(hasEqFunD "u_16_d" ''U_16 ''CInt   'Bitfields.eq_U_16_d)
$(hasEqFunD "u_16_e" ''U_16 ''CInt   'Bitfields.eq_U_16_e)
$(hasEqFunD "u_16_f" ''U_16 ''CInt   'Bitfields.eq_U_16_f)

test_U_16 :: TestTree
test_U_16 = testGroup "<=16-bit fields" [
      -- peek
      testProperty "peek @u_16_a" $ peek_prop @"u_16_a"
    , testProperty "peek @u_16_b" $ peek_prop @"u_16_b"
    , testProperty "peek @u_16_c" $ peek_prop @"u_16_c"
    , testProperty "peek @u_16_d" $ peek_prop @"u_16_d"
    , testProperty "peek @u_16_e" $ peek_prop @"u_16_e"
    , testProperty "peek @u_16_f" $ peek_prop @"u_16_f"

      -- poke
    , testProperty "poke @u_16_a" $ poke_prop @"u_16_a"
    , testProperty "poke @u_16_b" $ poke_prop @"u_16_b"
    , testProperty "poke @u_16_c" $ poke_prop @"u_16_c"
    , testProperty "poke @u_16_d" $ poke_prop @"u_16_d"
    , testProperty "poke @u_16_e" $ poke_prop @"u_16_e"
    , testProperty "poke @u_16_f" $ poke_prop @"u_16_f"
    ]
  where
    peek_prop ::
        forall fn. (
           U_16_C fn
         , HasSetFun (U_16 fn) (CBitfieldType Bitfields.U_16 fn)
         )
      => U_16 fn -> QC.Property
    peek_prop x = QC.ioProperty $ do
        y <- allocaAligned $ \ptr -> do
          setFun @(U_16 fn) @(CBitfieldType Bitfields.U_16 fn)
            ptr
            (getField @fn x)
          Foreign.peek ptr
        return $ y === x

    poke_prop ::
         forall fn. (
           U_16_C fn
         , HasEqFun (U_16 fn) (CBitfieldType Bitfields.U_16 fn)
         )
      => U_16 fn -> QC.Property
    poke_prop x = QC.ioProperty $ do
      isEq <- allocaAligned $ \ptr -> do
        Foreign.poke ptr x
        eqFun @(U_16 fn) @(CBitfieldType Bitfields.U_16 fn)
          ptr
          (getField @fn x)
      return $ isEq === C.CBool 1

type U_16_C fn = (
      HasField fn Bitfields.U_16 (CBitfieldType Bitfields.U_16 fn)
    , Eq (CBitfieldType Bitfields.U_16 fn)
    , Show (CBitfieldType Bitfields.U_16 fn)
    , KnownSymbol fn
    )

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.Bitfields.Unions" [
      testGroup "non-packed" [
          test_U_8
        , test_U_16
        ]
    ]
