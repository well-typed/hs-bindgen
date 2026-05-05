{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.MacroTypes (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
  ) where

import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CInt)
import Optics ((%~), (&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              testProperty)

import Generated.PointerManipulation qualified as Types (MyMacroType (..))
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.MacroTypes" [
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
     Func Types.MyMacroType
  -> Types.MyMacroType
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @Types.MyMacroType

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func Types.MyMacroType
  -> Types.MyMacroType
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @Types.MyMacroType

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary Types.MyMacroType where
  arbitrary = Types.MyMacroType <$> arbitrary
  shrink (Types.MyMacroType x) = Types.MyMacroType <$> shrink x

instance ComposableFunc Types.MyMacroType where
  newtype Func Types.MyMacroType = FuncMyMacroType {
      unwrapMyMacroType :: Fun CInt CInt
    }

  composed :: Func Types.MyMacroType -> Types.MyMacroType -> Types.MyMacroType
  composed f x =
      x & #unwrap %~ applyFun f.unwrapMyMacroType

  decomposed :: Func Types.MyMacroType -> [FieldFunc Types.MyMacroType]
  decomposed f = [
        FieldFunc (Proxy @"unwrap") (applyFun f.unwrapMyMacroType)
      ]

deriving stock instance Show (Func Types.MyMacroType)

instance Arbitrary (Func Types.MyMacroType) where
  arbitrary = FuncMyMacroType <$> arbitrary
  shrink (FuncMyMacroType x) = FuncMyMacroType <$> shrink x
