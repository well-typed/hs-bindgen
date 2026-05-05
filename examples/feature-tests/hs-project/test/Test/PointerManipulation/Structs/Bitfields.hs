{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Structs.Bitfields (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer
  , prop_applyPointer_equiv_applyPointerFields
  ) where

import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CUChar, CUInt)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Property, applyFun,
                              testProperty)

import Generated.PointerManipulation qualified as Types (MyStructBF (..))
import Generated.PointerManipulation.Safe qualified as Unsafe
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Structs.Bitfields" [
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
     Func Types.MyStructBF
  -> Types.MyStructBF
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @Types.MyStructBF

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func Types.MyStructBF
  -> Types.MyStructBF
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @Types.MyStructBF

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

mkStructBF :: CUInt -> CUChar -> Types.MyStructBF
mkStructBF x y = unsafePerformIO $ Unsafe.make_MyStructBF x y

instance Arbitrary Types.MyStructBF where
  arbitrary = mkStructBF <$> arbitrary <*> arbitrary
  shrink (Types.MyStructBF x y) = uncurry mkStructBF <$> shrink (x, y)

instance ComposableFunc Types.MyStructBF where
  data Func Types.MyStructBF = FuncMyStruct {
      myStruct_x :: Fun CUInt CUInt
    , myStruct_y :: Fun CUChar CUChar
    }

  composed :: Func Types.MyStructBF -> Types.MyStructBF -> Types.MyStructBF
  composed f struct = mkStructBF
                  (applyFun f.myStruct_x struct.x)
                  (applyFun f.myStruct_y struct.y)

  decomposed :: Func Types.MyStructBF -> [FieldFunc Types.MyStructBF]
  decomposed f = [
        BitfieldFunc (Proxy @"x") (applyFun f.myStruct_x)
      , BitfieldFunc (Proxy @"y") (applyFun f.myStruct_y)
      ]

deriving stock instance Show (Func Types.MyStructBF)

instance Arbitrary (Func Types.MyStructBF) where
  arbitrary = FuncMyStruct <$> arbitrary <*> arbitrary
  shrink (FuncMyStruct x y) = uncurry FuncMyStruct <$> shrink (x, y)
