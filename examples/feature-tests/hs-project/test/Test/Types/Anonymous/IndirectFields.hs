{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RoleAnnotations #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Tests for indirect fields
module Test.Types.Anonymous.IndirectFields (
    tests
  ) where

import Data.Function (on)
import Foreign.C.Types
import Test.Tasty
import Test.Tasty.QuickCheck

import HsBindgen.Runtime.Prelude (IsUnion)
import HsBindgen.Runtime.Support (HasField (..))
import HsBindgen.Runtime.Support.CompatHasField (modifyField)
import HsBindgen.Runtime.Support.CompatHasField qualified as Compat
import HsBindgen.Runtime.Union qualified as Union

import Generated.Types.Anonymous.IndirectFields
import Generated.Types.Anonymous.IndirectFields.Safe qualified as Safe
import Test.Util.Orphans ()
import Test.Util.TypedUnion

tests :: TestTree
tests = testGroup "Test.Types.Anonymous.IndirectFields" [
      testProperty "prop_SS" prop_SS
    , testProperty "prop_US" prop_US
    , testProperty "prop_SU @fieldX" $ prop_SU @"fieldX"
    , testProperty "prop_SU @fieldY" $ prop_SU @"fieldY"
    , testProperty "prop_UU @fieldX" $ prop_UU @"fieldX"
    , testProperty "prop_UU @fieldY" $ prop_UU @"fieldY"
    ]

{-------------------------------------------------------------------------------
  SS
-------------------------------------------------------------------------------}

instance Arbitrary SS where
  arbitrary =
    SS <$> (SS_anon'fieldX <$> arbitrary <*> arbitrary)
  shrink ((.anon'fieldX) -> x :: SS_anon'fieldX) =
      [ SS (SS_anon'fieldX fieldX' fieldY')
      | (fieldX', fieldY') <- shrink (x.fieldX, x.fieldY)
      ]

-- | Modifying via indirect fields is equivalent to modifying via direct fields
prop_SS :: SS -> Fun CInt CInt -> Fun CChar CChar -> Property
prop_SS x f g =
    ioProperty $
      (===) <$> Safe.show_SS baseline <*> Safe.show_SS feature
  where
    baseline =
        modifyField @"anon'fieldX" x $ \y ->
          modifyField @"fieldY"
            (modifyField @"fieldX" y (applyFun f))
            (applyFun g)

    feature =
        modifyField @"fieldY"
          (modifyField @"fieldX" x (applyFun f))
          (applyFun g)

{-------------------------------------------------------------------------------
  US
-------------------------------------------------------------------------------}

instance Arbitrary US where
  arbitrary =
    Union.set @"anon'fieldX" <$> (US_anon'fieldX <$> arbitrary <*> arbitrary)
  shrink ((.anon'fieldX) -> x :: US_anon'fieldX) =
      [ Union.set @"anon'fieldX" (US_anon'fieldX fieldX' fieldY')
      | (fieldX', fieldY') <- shrink (x.fieldX, x.fieldY)
      ]

instance Show US where
  show x = show x.anon'fieldX


-- | Modifying via indirect fields is equivalent to modifying via direct fields
prop_US :: US -> Fun CInt CInt -> Fun CChar CChar -> Property
prop_US x f g =
    ioProperty $
      (===) <$> Safe.show_US baseline <*> Safe.show_US feature
  where
    baseline =
        modifyField @"anon'fieldX" x $ \y ->
          modifyField @"fieldY"
            (modifyField @"fieldX" y (applyFun f))
            (applyFun g)

    feature =
        modifyField @"fieldY"
          (modifyField @"fieldX" x (applyFun f))
          (applyFun g)

{-------------------------------------------------------------------------------
  SU
-------------------------------------------------------------------------------}

instance IsUnion SU where
  zero = SU Union.zero

instance Arbitrary (TypedUnion SU "fieldX" CInt Field) where
  arbitrary = arbitraryField
  shrink = shrinkField

instance Arbitrary (TypedUnion SU "fieldY" CChar Field) where
  arbitrary = arbitraryField
  shrink = shrinkField

-- | Modifying via indirect fields is equivalent to modifying via direct fields
prop_SU ::
     forall fn ft. (
       Compat.HasField fn SU_anon'fieldX ft
     , Compat.HasField fn SU ft
     , HasField fn Str_repr ft
     , Eq ft
     , Show ft
     )
  => TypedUnion SU fn ft Field -> Fun ft ft -> Property
prop_SU x f =
    ioProperty $
      on (===) (getField @fn) <$> Safe.show_SU baseline <*> Safe.show_SU feature
  where
    baseline =
        modifyField @"anon'fieldX" (unsafeUnwrap x) $ \y ->
          modifyField @fn y (applyFun f)

    feature =
        modifyField @fn (unsafeUnwrap x) (applyFun f)

{-------------------------------------------------------------------------------
  UU
-------------------------------------------------------------------------------}

instance Arbitrary (TypedUnion UU "fieldX" CInt Field) where
  arbitrary = arbitraryField
  shrink = shrinkField

instance Arbitrary (TypedUnion UU "fieldY" CChar Field) where
  arbitrary = arbitraryField
  shrink = shrinkField

-- | Modifying via indirect fields is equivalent to modifying via direct fields
prop_UU ::
     forall fn ft. (
       Compat.HasField fn UU_anon'fieldX ft
     , Compat.HasField fn UU ft
     , HasField fn Str_repr ft
     , Eq ft
     , Show ft
     )
  => TypedUnion UU fn ft Field -> Fun ft ft -> Property
prop_UU x f =
    ioProperty $
      on (===) (getField @fn) <$> Safe.show_UU baseline <*> Safe.show_UU feature
  where
    baseline =
        modifyField @"anon'fieldX" (unsafeUnwrap x) $ \y ->
          modifyField @fn y (applyFun f)

    feature =
        modifyField @fn (unsafeUnwrap x) (applyFun f)
