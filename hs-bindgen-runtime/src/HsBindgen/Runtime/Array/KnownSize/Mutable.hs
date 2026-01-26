{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HsBindgen.Runtime.Array.KnownSize.Mutable (
    Array
  , withPtr
  ) where

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (Nat)

import HsBindgen.Runtime.HasFFIType (HasFFIType)
import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)

-- | A mutable C array of known size
newtype Array (n :: Nat) a = Array (Ptr a)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Storable)
  deriving newtype (ReadRaw, StaticSize, WriteRaw)
  deriving newtype (HasFFIType)

type role Array nominal nominal

-- | /( O(1) /): Retrieve the underlying pointer
withPtr :: Array n a -> (Ptr a -> IO r) -> IO r
withPtr (Array ptr) k = k ptr
