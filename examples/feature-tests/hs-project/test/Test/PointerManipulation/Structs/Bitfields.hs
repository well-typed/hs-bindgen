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
import Test.Tasty.QuickCheck (Arbitrary (..), Fun, Large (Large, getLarge),
                              Property, applyFun, testProperty)

import Generated.PointerManipulation qualified as Types (MyStructBF (..))
import Generated.PointerManipulation.Safe qualified as Safe
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

type MyStructBF = Types.MyStructBF

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer ::
     Func MyStructBF
  -> MyStructBF
  -> Property
prop_applyValue_equiv_applyPointer =
    Infra.prop_applyValue_equiv_applyPointer @MyStructBF

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields ::
     Func MyStructBF
  -> MyStructBF
  -> Property
prop_applyPointer_equiv_applyPointerFields =
    Infra.prop_applyPointer_equiv_applyPointerFields @MyStructBF

{-------------------------------------------------------------------------------
  Infra
-------------------------------------------------------------------------------}

mkStructBF :: CUInt -> CUChar -> MyStructBF
mkStructBF x y = unsafePerformIO $ Safe.make_MyStructBF x y

instance Arbitrary MyStructBF where
  arbitrary = mkStructBF <$> (getLarge <$> arbitrary) <*> (getLarge <$> arbitrary)
  shrink (Types.MyStructBF x y) =
      [ mkStructBF x' y'
      | (Large x', Large y') <- shrink (Large x, Large y)
      ]

instance ComposableFunc MyStructBF where
  data Func MyStructBF = FuncMyStructBF {
      x :: Fun CUInt CUInt
    , y :: Fun CUChar CUChar
    }

  composed :: Func MyStructBF -> MyStructBF -> MyStructBF
  composed f struct = mkStructBF
                  (applyFun f.x struct.x)
                  (applyFun f.y struct.y)

  decomposed :: Func MyStructBF -> [FieldFunc MyStructBF]
  decomposed f = [
        BitfieldFunc (Proxy @"x") (applyFun f.x)
      , BitfieldFunc (Proxy @"y") (applyFun f.y)
      ]

deriving stock instance Show (Func MyStructBF)

instance Arbitrary (Func MyStructBF) where
  arbitrary = FuncMyStructBF <$> arbitrary <*> arbitrary
  shrink (FuncMyStructBF x y) = uncurry FuncMyStructBF <$> shrink (x, y)
