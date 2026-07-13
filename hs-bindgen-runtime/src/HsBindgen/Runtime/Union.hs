{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Class for C unions
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Union qualified as Union
module HsBindgen.Runtime.Union (
    IsUnion (..)
  , get
  , set
  ) where

import Data.Coerce (coerce)
import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import GHC.Records.Compat qualified as Compat
import GHC.TypeNats (KnownNat)

import HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Support.SizedByteArray (SizedByteArray (..))

class IsUnion u where
  zero :: u

instance (KnownNat n, KnownNat m) => IsUnion (SizedByteArray n m) where
  zero = coerce $ BA.byteArrayFromListN n $ replicate n (0 :: Word8)
    where
      n = staticSizeOf (Proxy :: Proxy (SizedByteArray n m))

get ::
     forall field union a. Compat.HasField field union a
  => union
  -> a
get = Compat.getField @field

set ::
     forall field union a. (Compat.HasField field union a, IsUnion union)
  => a
  -> union
set = Compat.setField @field zero
