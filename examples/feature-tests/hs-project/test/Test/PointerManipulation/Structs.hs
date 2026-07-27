{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Structs (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
  ) where

import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CChar, CInt)
import Optics ((%~), (&))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              testProperty)

import Generated.PointerManipulation qualified as Types (MyStruct (..))
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Structs" [
      testProperty "prop_applyValue_equiv_applyPointer"
        prop_applyValue_equiv_applyPointer
    , testProperty "prop_applyPointer_equiv_applyPointerFields"
        prop_applyPointer_equiv_applyPointerFields
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

type MyStruct = Types.MyStruct

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer ::
     Func MyStruct
  -> MyStruct
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @MyStruct

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func MyStruct
  -> MyStruct
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @MyStruct

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

instance Arbitrary MyStruct where
  arbitrary = Types.MyStruct <$> arbitrary <*> arbitrary
  shrink (Types.MyStruct x y) =
      [ Types.MyStruct x' y' | (x', y') <- shrink (x, y) ]

instance ComposableFunc MyStruct where
  data Func MyStruct = FuncMyStruct {
      myStruct_x :: Fun CInt CInt
    , myStruct_y :: Fun CChar CChar
    }

  composed :: Func MyStruct -> MyStruct -> MyStruct
  composed f struct =
      struct & #x %~ applyFun f.myStruct_x
             & #y %~ applyFun f.myStruct_y

  decomposed :: Func MyStruct -> [FieldFunc MyStruct]
  decomposed f = [
        FieldFunc (Proxy @"x") (applyFun f.myStruct_x)
      , FieldFunc (Proxy @"y") (applyFun f.myStruct_y)
      ]

deriving stock instance Show (Func MyStruct)

instance Arbitrary (Func MyStruct) where
  arbitrary = FuncMyStruct <$> arbitrary <*> arbitrary
  shrink (FuncMyStruct x y) =
      [ FuncMyStruct x' y'
      | (x', y') <- shrink (x, y)
      ]
