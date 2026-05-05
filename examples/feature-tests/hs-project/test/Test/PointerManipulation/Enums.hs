{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Enums (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
  ) where

import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CUInt)
import Optics ((%~), (&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              testProperty)

import Generated.PointerManipulation qualified as Types (MyEnum (..))
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Enums" [
      testProperty "prop_applyValue_equiv_applyPointer"
        prop_applyValue_equiv_applyPointer
    , testProperty "prop_applyPointer_equiv_applyPointerFields"
        prop_applyPointer_equiv_applyPointerFields
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer ::
     Func Types.MyEnum
  -> Types.MyEnum
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @Types.MyEnum

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func Types.MyEnum
  -> Types.MyEnum
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @Types.MyEnum

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary Types.MyEnum where
  arbitrary = Types.MyEnum <$> arbitrary
  shrink (Types.MyEnum x) = Types.MyEnum <$> shrink x

instance ComposableFunc Types.MyEnum where
  newtype Func Types.MyEnum = FuncMyEnum {
      unwrapMyEnum :: Fun CUInt CUInt
    }

  composed :: Func Types.MyEnum -> Types.MyEnum -> Types.MyEnum
  composed f x =
      x & #unwrap %~ applyFun f.unwrapMyEnum

  decomposed :: Func Types.MyEnum -> [FieldFunc Types.MyEnum]
  decomposed f = [
        FieldFunc (Proxy @"unwrap") (applyFun f.unwrapMyEnum)
      ]

deriving stock instance Show (Func Types.MyEnum)

instance Arbitrary (Func Types.MyEnum) where
  arbitrary = FuncMyEnum <$> arbitrary
  shrink (FuncMyEnum x) = FuncMyEnum <$> shrink x
