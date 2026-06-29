{-# LANGUAGE RoleAnnotations #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.PointerManipulation.Unions (
    tests
    -- * Properties (exported for haddocks)
  , prop_applyValue_equiv_applyPointer_unions
  , prop_applyPointer_equiv_applyPointerFields_unions
  ) where

import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Foreign.C.Types (CChar, CInt)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import GHC.Records (HasField (getField))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Internal.Prelude.CompatHasField qualified as Compat
import HsBindgen.Runtime.Union qualified as Union

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
     Func (MyUnion "x" CInt)
  -> MyUnion "x" CInt
  -> Property
prop_applyValue_equiv_applyPointer_unions =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions ::
     Func (MyUnion "x" CInt)
  -> MyUnion "x" CInt
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions =
    Infra.prop_applyPointer_equiv_applyPointerFields

-- | See 'Infra.prop_applyValue_equiv_applyPointer'
prop_applyValue_equiv_applyPointer_unions_2 ::
     Func (MyUnion "y" CChar)
  -> MyUnion "y" CChar
  -> Property
prop_applyValue_equiv_applyPointer_unions_2 =
    Infra.prop_applyValue_equiv_applyPointer

-- | See 'Infra.prop_applyPointer_equiv_applyPointerFields'
prop_applyPointer_equiv_applyPointerFields_unions_2 ::
     Func (MyUnion "y" CChar)
  -> MyUnion "y" CChar
  -> Property
prop_applyPointer_equiv_applyPointerFields_unions_2 =
    Infra.prop_applyPointer_equiv_applyPointerFields

{-------------------------------------------------------------------------------
  Infra: MyUnionX
-------------------------------------------------------------------------------}

type role MyUnion nominal nominal
newtype MyUnion f ft = MyUnion Types.MyUnion
  deriving newtype Storable

instance HasField f Types.MyUnion ft
      => HasField f (MyUnion f ft) ft where
  getField (MyUnion x) = getField @f x

instance Compat.HasField f Types.MyUnion ft
      => Compat.HasField f (MyUnion f ft) ft where
  hasField (MyUnion x) = coerce $ Compat.hasField @f x

instance HasField f (Ptr Types.MyUnion) (Ptr ft)
      => HasField f (Ptr (MyUnion f ft)) (Ptr ft) where
  getField ptr = getField @f (castPtr @(MyUnion f ft) @Types.MyUnion ptr)

instance (HasField f Types.MyUnion ft, Eq ft)
      => Eq (MyUnion f ft) where
  MyUnion x == MyUnion y = getField @f x == getField @f y

instance (HasField f Types.MyUnion ft, Show ft)
      => Show (MyUnion f ft) where
  show (MyUnion x) = show (getField @f x)

instance ( Compat.HasField f Types.MyUnion ft
         , Arbitrary ft
         )
      => Arbitrary (MyUnion f ft) where
  arbitrary = MyUnion  . Union.set @f <$> arbitrary
  shrink (MyUnion x) = MyUnion . Union.set @f <$> shrink (Compat.getField @f x)

instance ( HasField f Types.MyUnion ft
         , Compat.HasField f Types.MyUnion ft
         , Storable ft
         , HasField f (Ptr Types.MyUnion) (Ptr ft)
         ) => ComposableFunc (MyUnion f ft) where
  data Func (MyUnion f ft) = MyUnionFunc {
      unwrap :: Fun ft ft
    }

  composed :: Func (MyUnion f ft) -> (MyUnion f ft) -> (MyUnion f ft)
  composed f (MyUnion x) = MyUnion $ Union.set  @f $ applyFun f.unwrap $ Compat.getField @f x

  decomposed :: Func (MyUnion f ft) -> [FieldFunc (MyUnion f ft)]
  decomposed f = [
      FieldFunc (Proxy @f) (applyFun f.unwrap)
    ]

deriving stock instance Show ft => Show (Func (MyUnion f ft))

instance ( Function ft
         , Arbitrary ft
         , CoArbitrary ft
         ) => Arbitrary (Func (MyUnion f ft)) where
  arbitrary = MyUnionFunc <$> arbitrary
  shrink (MyUnionFunc x) = MyUnionFunc <$> shrink x
