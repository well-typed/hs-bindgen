{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Class for C unions
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Union qualified as Union
module HsBindgen.Runtime.Union (
    IsUnion (..)
  , IsUnionViaStorable (..)
  , get
  , set
  ) where

import Data.Coerce (coerce)
import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (peek, sizeOf))
import GHC.Records.Compat qualified as Compat
import GHC.TypeNats (KnownNat)
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Runtime.Marshal (StaticSize (staticSizeOf))
import HsBindgen.Runtime.Support.SizedByteArray (SizedByteArray (..))

class IsUnion u where
  -- | A 'zero' union value has all the bytes of its fields set to 0
  --
  -- If the union has nested struct or union values, then all the bytes of their
  -- fields are also set to 0.
  --
  -- The following equivalence should hold:
  --
  -- * A 'zero' union value is equal to a union value that is 'peek'ed from a
  --   zeroed-out byte array
  zero :: u

-- | Helper type for deriving 'IsUnion' via 'Storable'
newtype IsUnionViaStorable s = IsUnionViaStorable s

-- | Helper instance for deriving 'IsUnion' via 'Storable'
--
-- This instance is equivalent to the helper instance for newtype-deriving, but
-- the latter is probably more performant.
instance Storable s => IsUnion (IsUnionViaStorable s) where
  zero =
      unsafePerformIO $
      BA.withByteArrayContents zeroBytes $ \(ptr :: Ptr Word8) ->
        IsUnionViaStorable <$> peek (castPtr ptr :: Ptr ( s))
    where
      n = sizeOf (undefined :: s)
      zeroBytes = BA.byteArrayFromListN n $ replicate n (0 :: Word8)

-- | Helper instance for newtype-deriving 'IsUnion'
--
-- This instance is equivalent to the helper instance for deriving via
-- 'Storable', but the former is probably more performant.
instance (KnownNat n, KnownNat m) => IsUnion (SizedByteArray n m) where
  zero = coerce zeroBytes
    where
      n = staticSizeOf (Proxy :: Proxy (SizedByteArray n m))
      zeroBytes = BA.byteArrayFromListN n $ replicate n (0 :: Word8)

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
