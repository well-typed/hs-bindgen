{-# LANGUAGE DataKinds #-}

-- | Tests for nested anonymous objects and implicit field generation
module Test.Types.Anonymous (
    tests
  ) where

import Data.Kind (Type)
import Data.Proxy
import Foreign.C.Types (CSize)
import GHC.TypeLits (Symbol)
import Test.Tasty
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.HasCField qualified as HasCField
import HsBindgen.Runtime.Prelude

import Generated.Types.Anonymous qualified as Types
import Generated.Types.Anonymous.Global qualified as Global

-- | @hs-bindgen@ generates bindings for nested anonymous objects.
tests :: TestTree
tests = testGroup "Test.Types.Anonymous" [
      testProperty "prop_offsets_structInStruct" prop_offsets_structInStruct
    , testProperty "prop_offsets_unionInStruct"  prop_offsets_unionInStruct
    , testProperty "prop_offsets_structInUnion"  prop_offsets_structInUnion
    , testProperty "prop_offsets_unionInUnion"   prop_offsets_unionInUnion
    ]

-- | Test 'prop_offsets' for a struct declaration nested in a struct declaration
prop_offsets_structInStruct :: Property
prop_offsets_structInStruct =
    prop_offsets
      (Proxy @Types.SS1, Proxy @"sS1_fieldX", Global.offset_SS1_fieldX)
      (Proxy @Types.SS2, Proxy @"sS2_fieldB", Global.offset_SS2_fieldB)

-- | Test 'prop_offsets' for a union declaration nested in a struct declaration
prop_offsets_unionInStruct :: Property
prop_offsets_unionInStruct =
    prop_offsets
      (Proxy @Types.SU1, Proxy @"sU1_fieldX", Global.offset_SU1_fieldX)
      (Proxy @Types.SU2, Proxy @"sU2_fieldB", Global.offset_SU2_fieldB)

-- | Test 'prop_offsets' for a struct declaration nested in a union declaration
prop_offsets_structInUnion :: Property
prop_offsets_structInUnion =
    prop_offsets
      (Proxy @Types.US1, Proxy @"uS1_fieldX", Global.offset_US1_fieldX)
      (Proxy @Types.US2, Proxy @"uS2_fieldB", Global.offset_US2_fieldB)

-- | Test 'prop_offsets' for a union declaration nested in a union declaration
prop_offsets_unionInUnion :: Property
prop_offsets_unionInUnion =
    prop_offsets
      (Proxy @Types.UU1, Proxy @"uU1_fieldX", Global.offset_UU1_fieldX)
      (Proxy @Types.UU2, Proxy @"uU2_fieldB", Global.offset_UU2_fieldB)

-- | Test that the offsets for generated implicit fields are computed correctly.
--
-- Implicit fields are generated for nested anonymous structs and unions.
--
-- Concretely, we:
--
-- * Test that the offset for an implicit field is the same in C as the
--   generated Haskell bindings.
--
-- * Test that the offset for a generated implicit field is the same as the
--   offset for a an equivalent parsed explicit field.
--
prop_offsets ::
     forall (t1 :: Type) (field1 :: Symbol) (t2 :: Type) (field2 :: Symbol).
     (HasCField t1 field1, HasCField t2 field2)
  => (Proxy t1, Proxy field1, CSize)
  -> (Proxy t2, Proxy field2, CSize)
  -> Property
prop_offsets (ps1, pf1, off1) (ps2, pf2, off2) = conjoin [
      counterexample "a" $ HasCField.offset ps1 pf1 === fromIntegral off1
    , counterexample "b" $ HasCField.offset ps2 pf2 === fromIntegral off2
    , counterexample "c" $ off1 === off2
    ]
