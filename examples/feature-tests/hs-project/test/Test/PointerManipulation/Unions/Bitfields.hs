{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Unions.Bitfields (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer_x
  , prop_applyPointer_equiv_applyPointerFields_x
  , prop_applyValue_equiv_applyPointer_y
  , prop_applyPointer_equiv_applyPointerFields_y
  ) where

import Data.Proxy (Proxy (..))
import Foreign.C.Types (CUChar, CUInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Large (getLarge), Property,
                              applyFun, testProperty)

import HsBindgen.Runtime.Overloading (Show, fromString, getField, setField,
                                      (<$>))

import Generated.PointerManipulation qualified as Types (MyUnionBF (..))
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()
import Test.Util.TypedUnion (FieldKind (..), TypedUnion, liftArbitraryField,
                             shrinkField)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Unions.Bitfields" [
      testProperty "prop_applyValue_equiv_applyPointer_x"
        prop_applyValue_equiv_applyPointer_x
    , testProperty "prop_applyPointer_equiv_applyPointerFields_x"
        prop_applyPointer_equiv_applyPointerFields_x

    , testProperty "prop_applyValue_equiv_applyPointer_y"
        prop_applyValue_equiv_applyPointer_y
    , testProperty "prop_applyPointer_equiv_applyPointerFields_y"
        prop_applyPointer_equiv_applyPointerFields_y
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

type MyUnionBF fn ft = TypedUnion Types.MyUnionBF fn ft Bitfield

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_x ::
     Func (MyUnionBF "x" CUInt)
  -> MyUnionBF "x" CUInt
  -> Property
prop_applyValue_equiv_applyPointer_x =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_x ::
     Func (MyUnionBF "x" CUInt)
  -> MyUnionBF "x" CUInt
  -> Property
prop_applyPointer_equiv_applyPointerFields_x =
    Infra.prop_applyPointer_equiv_applyPointerFields

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_y ::
     Func (MyUnionBF "y" CUChar)
  -> MyUnionBF "y" CUChar
  -> Property
prop_applyValue_equiv_applyPointer_y =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_y ::
     Func (MyUnionBF "y" CUChar)
  -> MyUnionBF "y" CUChar
  -> Property
prop_applyPointer_equiv_applyPointerFields_y =
    Infra.prop_applyPointer_equiv_applyPointerFields

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary (MyUnionBF "x" CUInt) where
  arbitrary = liftArbitraryField (getLarge <$> arbitrary)
  shrink = shrinkField

instance Arbitrary (MyUnionBF "y" CUChar) where
  arbitrary = liftArbitraryField (getLarge <$> arbitrary)
  shrink = shrinkField

instance ComposableFunc (MyUnionBF "x" CUInt) where
  data Func (MyUnionBF "x" CUInt) = FuncMyUnionBF_x {
      x :: Fun CUInt CUInt
    }

  composed :: Func (MyUnionBF "x" CUInt) -> MyUnionBF "x" CUInt -> MyUnionBF "x" CUInt
  composed f union = union { x = applyFun f.x union.x }

  decomposed :: Func (MyUnionBF "x" CUInt) -> [FieldFunc (MyUnionBF "x" CUInt)]
  decomposed f = [
        BitfieldFunc (Proxy @"x") (applyFun f.x)
      ]

deriving stock instance Show (Func (MyUnionBF "x" CUInt))

instance Arbitrary (Func (MyUnionBF "x" CUInt)) where
  arbitrary = FuncMyUnionBF_x <$> arbitrary
  shrink (FuncMyUnionBF_x x) = FuncMyUnionBF_x <$> shrink x

instance ComposableFunc (MyUnionBF "y" CUChar) where
  data Func (MyUnionBF "y" CUChar) = FuncMyUnionBF_y {
      y :: Fun CUChar CUChar
    }

  composed :: Func (MyUnionBF "y" CUChar) -> MyUnionBF "y" CUChar -> MyUnionBF "y" CUChar
  composed f union = union { y = applyFun f.y union.y }

  decomposed :: Func (MyUnionBF "y" CUChar) -> [FieldFunc (MyUnionBF "y" CUChar)]
  decomposed f = [
        BitfieldFunc (Proxy @"y") (applyFun f.y)
      ]

deriving stock instance Show (Func (MyUnionBF "y" CUChar))

instance Arbitrary (Func (MyUnionBF "y" CUChar)) where
  arbitrary = FuncMyUnionBF_y <$> arbitrary
  shrink (FuncMyUnionBF_y x) = FuncMyUnionBF_y <$> shrink x
