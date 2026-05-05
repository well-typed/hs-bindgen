{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Typedefs (
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

import Generated.PointerManipulation qualified as Types (MyTypedef (..))
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Typedefs" [
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
     Func Types.MyTypedef
  -> Types.MyTypedef
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @Types.MyTypedef

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func Types.MyTypedef
  -> Types.MyTypedef
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @Types.MyTypedef

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary Types.MyTypedef where
  arbitrary = Types.MyTypedef <$> arbitrary
  shrink (Types.MyTypedef x) = Types.MyTypedef <$> shrink x

instance ComposableFunc Types.MyTypedef where
  newtype Func Types.MyTypedef = FuncMyTypedef {
      unwrapMyTypedef :: Fun CInt CInt
    }

  composed :: Func Types.MyTypedef -> Types.MyTypedef -> Types.MyTypedef
  composed f x =
      x & #unwrap %~ applyFun f.unwrapMyTypedef

  decomposed :: Func Types.MyTypedef -> [FieldFunc Types.MyTypedef]
  decomposed f = [
        FieldFunc (Proxy @"unwrap") (applyFun f.unwrapMyTypedef)
      ]

deriving stock instance Show (Func Types.MyTypedef)

instance Arbitrary (Func Types.MyTypedef) where
  arbitrary = FuncMyTypedef <$> arbitrary
  shrink (FuncMyTypedef x) = FuncMyTypedef <$> shrink x
