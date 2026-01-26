{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module HsBindgen.Runtime.Array.UnknownSize.Const.Mutable (
    Array
  , withPtr
  ) where

import Prelude hiding (repeat)

import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.TypeNats (Nat)

import HsBindgen.Runtime.ConstPtr (ConstPtr (..))
import HsBindgen.Runtime.HasFFIType (HasFFIType)
import HsBindgen.Runtime.Marshal (ReadRaw, StaticSize, WriteRaw)

-- | A mutable C array of known size
newtype Array (n :: Nat) a = Array (ConstPtr a)
  deriving stock (Show, Eq, Ord)
  deriving newtype (Storable)
  deriving newtype (HasFFIType)
  deriving ReadRaw via Ptr a
  deriving StaticSize via Ptr a
  deriving WriteRaw via Ptr a

type role Array nominal nominal

-- | /( O(1) /): Retrieve the underlying pointer
withPtr :: Array n a -> (ConstPtr a -> IO r) -> IO r
withPtr (Array ptr) k = k ptr
