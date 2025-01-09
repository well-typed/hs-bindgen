{-# LANGUAGE MagicHash #-}
module HsBindgen.Runtime.FlexibleArrayMember (
    HasFlexibleArrayMember (..),
    HasFlexibleArrayLength (..),
    WithFlexbileArrayMember (..),
) where

import GHC.Exts (Proxy#)
import Data.Vector.Storable qualified as VS

class HasFlexibleArrayMember b a | a -> b where
  flexibleArrayMemberOffset :: Proxy# a -> Int

class HasFlexibleArrayMember b a => HasFlexibleArrayLength b a | a -> b where
  flexibleArrayMemberLength :: a -> Int

data WithFlexbileArrayMember b a = WithFlexbileArrayMember !a {-# UNPACK #-} !(VS.Vector b)

{-
-- Single instance, defined once and for all
instance (Storable a, HasFlexibleArrayLength b a) => Storable (WithFlexbileArrayMember b a) where
    alignment _ = aligment (a :: undefined)
    sizeof _ = sizeof (a :: undefined)
-}
