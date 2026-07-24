{-# LANGUAGE RoleAnnotations #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Unions (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer_unions_x
  , prop_applyPointer_equiv_applyPointerFields_unions_x
  , prop_applyValue_equiv_applyPointer_unions_y
  , prop_applyPointer_equiv_applyPointerFields_unions_y
  ) where

import Foreign.C.Types (CChar, CInt)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, testProperty)

import Generated.PointerManipulation qualified as Types
import Test.PointerManipulation.Infra (ComposableFunc (Func))
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()
import Test.Util.TypedUnion (FieldKind (..), TypedUnion, arbitraryField,
                             shrinkField)

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Unions" [
      testProperty "prop_applyValue_equiv_applyPointer_unions_x"
        prop_applyValue_equiv_applyPointer_unions_x
    , testProperty "prop_applyPointer_equiv_applyPointerFields_unions_x"
        prop_applyPointer_equiv_applyPointerFields_unions_x

    , testProperty "prop_applyValue_equiv_applyPointer_unions_y"
        prop_applyValue_equiv_applyPointer_unions_y
    , testProperty "prop_applyPointer_equiv_applyPointerFields_unions_y"
        prop_applyPointer_equiv_applyPointerFields_unions_y
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

type MyUnion fn ft = TypedUnion Types.MyUnion fn ft Field

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_unions_x ::
     Func (MyUnion "x" CInt)
  -> MyUnion "x" CInt
  -> Property
prop_applyValue_equiv_applyPointer_unions_x =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions_x ::
     Func (MyUnion "x" CInt)
  -> MyUnion "x" CInt
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions_x =
    Infra.prop_applyPointer_equiv_applyPointerFields

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_unions_y ::
     Func (MyUnion "y" CChar)
  -> MyUnion "y" CChar
  -> Property
prop_applyValue_equiv_applyPointer_unions_y =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions_y ::
     Func (MyUnion "y" CChar)
  -> MyUnion "y" CChar
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions_y =
    Infra.prop_applyPointer_equiv_applyPointerFields

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary (MyUnion "x" CInt) where
  arbitrary = arbitraryField
  shrink = shrinkField

instance Arbitrary (MyUnion "y" CChar) where
  arbitrary = arbitraryField
  shrink = shrinkField
