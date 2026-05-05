{-# LANGUAGE RoleAnnotations #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Unions (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer_unions
  , prop_applyPointer_equiv_applyPointerFields_unions
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CChar, CInt)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import GHC.Records (HasField (getField))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Generated.PointerManipulation qualified as Types
import Test.PointerManipulation.Infra (ComposableFunc, FieldFunc (..), Func)
import Test.PointerManipulation.Infra qualified as Infra
import Test.Util.Orphans ()

{-------------------------------------------------------------------------------
  Tests
-------------------------------------------------------------------------------}

tests :: TestTree
tests = testGroup "Test.PointerManipulation.Unions" [
      testProperty "prop_applyValue_equiv_applyPointer_unions"
        prop_applyValue_equiv_applyPointer_unions
    , testProperty "prop_applyPointer_equiv_applyPointerFields_unions"
        prop_applyPointer_equiv_applyPointerFields_unions

    , testProperty "prop_applyValue_equiv_applyPointer_unions_2"
        prop_applyValue_equiv_applyPointer_unions_2
    , testProperty "prop_applyPointer_equiv_applyPointerFields_unions_2"
        prop_applyPointer_equiv_applyPointerFields_unions_2
    ]

{-------------------------------------------------------------------------------
  Properties
-------------------------------------------------------------------------------}

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_unions ::
     Func (MyUnion "x")
  -> MyUnion "x"
  -> Property
prop_applyValue_equiv_applyPointer_unions =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions ::
     Func (MyUnion "x")
  -> MyUnion "x"
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions =
    Infra.prop_applyPointer_equiv_applyPointerFields

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_unions_2 ::
     Func (MyUnion "y")
  -> MyUnion "y"
  -> Property
prop_applyValue_equiv_applyPointer_unions_2 =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions_2 ::
     Func (MyUnion "y")
  -> MyUnion "y"
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions_2 =
    Infra.prop_applyPointer_equiv_applyPointerFields

{-------------------------------------------------------------------------------
  Infra: MyUnionX
-------------------------------------------------------------------------------}

instance HasField "x" Types.MyUnion CInt where
  getField = Types.get_myUnion_x

instance HasField "y" Types.MyUnion CChar where
  getField = Types.get_myUnion_y

class HasField f a (FieldType f a)
   => HasFieldAux f a where
  type FieldType f a :: Type
  setField :: Proxy f -> FieldType f a -> a

instance HasFieldAux "x" Types.MyUnion where
  type FieldType "x" Types.MyUnion = CInt
  setField _ x = Types.set_myUnion_x x

instance HasFieldAux "y" Types.MyUnion where
  type FieldType "y" Types.MyUnion = CChar
  setField _ x = Types.set_myUnion_y x

type role MyUnion nominal
newtype MyUnion f = MyUnion Types.MyUnion
  deriving newtype Storable

instance HasField f Types.MyUnion ft
      => HasField f (MyUnion f) ft where
  getField (MyUnion x) = getField @f x

instance HasField f (Ptr Types.MyUnion) (Ptr ft)
      => HasField f (Ptr (MyUnion f)) (Ptr ft) where
  getField ptr = getField @f (castPtr @(MyUnion f) @Types.MyUnion ptr)

instance (HasField f Types.MyUnion ft, Eq ft)
      => Eq (MyUnion f) where
  MyUnion x == MyUnion y = getField @f x == getField @f y

instance (HasField f Types.MyUnion ft, Show ft)
      => Show (MyUnion f) where
  show (MyUnion x) = show (getField @f x)

instance (HasFieldAux f Types.MyUnion, Arbitrary (FieldType f Types.MyUnion))
      => Arbitrary (MyUnion f) where
  arbitrary = MyUnion  . setField (Proxy @f) <$> arbitrary
  shrink (MyUnion x) = MyUnion . setField (Proxy @f) <$> shrink (getField @f x)

instance ( HasField f Types.MyUnion (FieldType f Types.MyUnion)
         , HasFieldAux f Types.MyUnion
         , Storable (FieldType f Types.MyUnion)
         , HasField f (Ptr Types.MyUnion) (Ptr (FieldType f Types.MyUnion))
         ) => ComposableFunc (MyUnion f) where
  data Func (MyUnion f) = MyUnionFunc {
      unwrap :: Fun (FieldType f Types.MyUnion) (FieldType f Types.MyUnion)
    }

  composed :: Func (MyUnion f) -> (MyUnion f) -> (MyUnion f)
  composed f (MyUnion x) = MyUnion $ setField (Proxy @f) $ applyFun f.unwrap $ getField @f x

  decomposed :: Func (MyUnion f) -> [FieldFunc (MyUnion f)]
  decomposed f = [
      FieldFunc (Proxy @f) (applyFun f.unwrap)
    ]

deriving stock instance Show (FieldType f Types.MyUnion) => Show (Func (MyUnion f))

instance ( Function (FieldType f Types.MyUnion)
         , Arbitrary (FieldType f Types.MyUnion)
         , CoArbitrary (FieldType f Types.MyUnion)
         ) => Arbitrary (Func (MyUnion f)) where
  arbitrary = MyUnionFunc <$> arbitrary
  shrink (MyUnionFunc x) = MyUnionFunc <$> shrink x
