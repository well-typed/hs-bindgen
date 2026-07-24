{-# LANGUAGE RoleAnnotations #-}

module Test.Util.TypedUnion (
    -- * Types
    TypedUnion
  , FieldKind (..)
  , unsafeWrap
  , unsafeUnwrap
    -- * Arbitrary
  , arbitraryField
  , shrinkField
  ) where

import Data.Coerce (coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable)
import GHC.Records (HasField (..))
import Test.QuickCheck (Arbitrary (..), CoArbitrary, Fun, Function, Gen,
                        applyFun)

import HsBindgen.Runtime.BitfieldPtr (BitfieldPtr)
import HsBindgen.Runtime.Prelude (IsUnion)
import HsBindgen.Runtime.Support.CompatHasField qualified as Compat
import HsBindgen.Runtime.Union qualified as Union

import Test.PointerManipulation.Infra (ComposableFunc (..),
                                       FieldFunc (FieldFunc))

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

-- | A typed union tracks which alternative it uses on the type level
type TypedUnion :: Type -> k1 -> k2 -> FieldKind -> Type
type role TypedUnion nominal nominal nominal nominal
newtype TypedUnion
      u {- union -}
      fn {- fieldName -}
      ft {- fieldType -}
      fk {- fieldKind -} =
    TypedUnion u
  deriving newtype Storable

data FieldKind = Bitfield | Field

unsafeWrap :: u -> TypedUnion u fn ft fk
unsafeWrap = TypedUnion

unsafeUnwrap :: TypedUnion u fn ft fk -> u
unsafeUnwrap (TypedUnion x) = x

{-------------------------------------------------------------------------------
  Instances
-------------------------------------------------------------------------------}

instance (HasField fn u ft, Eq ft)
      => Eq (TypedUnion u fn ft fk) where
  TypedUnion x == TypedUnion y = getField @fn x == getField @fn y

instance (HasField fn u ft, Show ft)
      => Show (TypedUnion u fn ft fk) where
  show (TypedUnion x) = show (getField @fn x)

{-------------------------------------------------------------------------------
  Instances: HasField
-------------------------------------------------------------------------------}

instance HasField fn u ft
      => HasField fn (TypedUnion u fn ft fk) ft where
  getField (TypedUnion x) = getField @fn x

instance Compat.HasField fn u ft
      => Compat.HasField fn (TypedUnion u fn ft fk) ft where
  hasField (TypedUnion x) = coerce $ Compat.hasField @fn x

{-------------------------------------------------------------------------------
  Instances: pointer manipulation API
-------------------------------------------------------------------------------}

instance HasField fn (Ptr u) (Ptr ft)
      => HasField fn (Ptr (TypedUnion u fn ft Field)) (Ptr ft) where
  getField ptr = getField @fn (castPtr @(TypedUnion u fn ft Field) @u ptr)

instance HasField fn (Ptr u) (BitfieldPtr ft)
      => HasField fn (Ptr (TypedUnion u fn ft Bitfield)) (BitfieldPtr ft) where
  getField ptr = getField @fn (castPtr @(TypedUnion u fn ft Bitfield) @u ptr)

{-------------------------------------------------------------------------------
  Arbitrary
-------------------------------------------------------------------------------}

arbitraryField::
     forall fn u ft fk. (
       Compat.HasField fn u ft
     , Arbitrary ft
     , IsUnion u
     )
  => Gen (TypedUnion u fn ft fk)
arbitraryField  = TypedUnion . Union.set @fn <$> arbitrary

shrinkField ::
     forall fn u ft fk. (
       Compat.HasField fn u ft
     , Arbitrary ft
     , IsUnion u
     )
  => TypedUnion u fn ft fk
  -> [TypedUnion u fn ft fk]
shrinkField (TypedUnion x) = TypedUnion . Union.set @fn <$> shrink (Compat.getField @fn x)

{-------------------------------------------------------------------------------
  ComposableFunc
-------------------------------------------------------------------------------}

instance ( HasField fn u ft
         , Compat.HasField fn u ft
         , Storable ft
         , HasField fn (Ptr u) (Ptr ft)
         , IsUnion u
         ) => ComposableFunc (TypedUnion u fn ft Field) where
  data Func (TypedUnion u fn ft Field) = TypedUnionFunc {
      unwrap :: Fun ft ft
    }

  composed :: Func (TypedUnion u fn ft Field) -> TypedUnion u fn ft Field -> TypedUnion u fn ft Field
  composed f (TypedUnion x) = TypedUnion $ Union.set @fn $ applyFun f.unwrap $ Compat.getField @fn x

  decomposed :: Func (TypedUnion u fn ft Field) -> [FieldFunc (TypedUnion u fn ft Field)]
  decomposed f = [
      FieldFunc (Proxy @fn) (applyFun f.unwrap)
    ]

deriving stock instance Show ft => Show (Func (TypedUnion u fn ft Field))

instance ( Function ft
         , Arbitrary ft
         , CoArbitrary ft
         ) => Arbitrary (Func (TypedUnion u fn ft Field)) where
  arbitrary = TypedUnionFunc <$> arbitrary
  shrink (TypedUnionFunc x) = TypedUnionFunc <$> shrink x
