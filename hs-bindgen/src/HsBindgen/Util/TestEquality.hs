{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}

module HsBindgen.Util.TestEquality
  ( SimpleEnum(..) ) where

-- base
import Data.Kind
  ( Type )
import Data.Type.Equality
  ( (:~:)(..), TestEquality(..) )
import GHC.Exts
  ( (==#), dataToTag#, isTrue#
#if MIN_VERSION_base(4,20,0)
  , DataToTag
#endif
  )
import Unsafe.Coerce
  ( unsafeCoerce )

--------------------------------------------------------------------------------

-- | Newtype for deriving 'TestEquality' on simple enumeration GADTs.
type SimpleEnum :: forall {k}. ( k -> Type ) -> k -> Type
newtype SimpleEnum a tag = SimpleEnum ( a tag )

instance
#if MIN_VERSION_base(4,20,0)
  ( forall tag. DataToTag ( a tag ) ) =>
#endif
  TestEquality ( SimpleEnum a ) where
  testEquality ( SimpleEnum k1 ) ( SimpleEnum k2 )
    | isTrue# ( dataToTag# k1 ==# dataToTag# k2 )
    = Just $ unsafeCoerce Refl
    | otherwise
    = Nothing
