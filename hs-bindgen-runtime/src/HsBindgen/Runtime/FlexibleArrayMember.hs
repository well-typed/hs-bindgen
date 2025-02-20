{-# LANGUAGE MagicHash #-}
module HsBindgen.Runtime.FlexibleArrayMember (
    HasFlexibleArrayMember (..),
    HasFlexibleArrayLength (..),
    WithFlexbileArrayMember (..),
) where

import GHC.Exts (Proxy#)
import Data.Vector.Storable qualified as VS

class HasFlexibleArrayMember element struct | struct -> element where
  flexibleArrayMemberOffset :: Proxy# struct -> Int

class HasFlexibleArrayMember element struct => HasFlexibleArrayLength element struct | struct -> element where
  flexibleArrayMemberLength :: struct -> Int

data WithFlexbileArrayMember element struct = WithFlexbileArrayMember !struct {-# UNPACK #-} !(VS.Vector element)

{-
-- Single instance, defined once and for all
instance (Storable a, HasFlexibleArrayLength b a) => Storable (WithFlexbileArrayMember b a) where
    alignment _ = aligment (a :: undefined)
    sizeof _ = sizeof (a :: undefined)
-}
