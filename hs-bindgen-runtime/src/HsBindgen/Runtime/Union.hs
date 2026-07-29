{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Class for C unions
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Union qualified as Union
module HsBindgen.Runtime.Union (
    IsUnion (..)
  , IsUnionViaReadRaw (..)
  , get
  , set
  ) where

import Data.Coerce (coerce)
import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Records.Compat qualified as Compat
import GHC.TypeNats (KnownNat)
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Runtime.Marshal (ReadRaw (..), StaticSize (staticSizeOf))
import HsBindgen.Runtime.Support.SizedByteArray (SizedByteArray (..))

class IsUnion u where
  -- | A 'zero' union value is a union value that is read from a zeroed-out byte
  -- array
  zero :: u

-- | Helper type for deriving 'IsUnion' via 'ReadRaw' (and 'StaticSize')
--
-- This helper type exists mainly for user convenience so that they can define
-- instances in cases where newtype-deriving is not possible.
newtype IsUnionViaReadRaw u = IsUnionViaReadRaw u

-- | Helper instance for deriving 'IsUnion' via 'ReadRaw' (and 'StaticSize')
--
-- This instance equivalent to the helper instance for newtype-deriving, but the
-- latter is probably more performant.
instance (StaticSize u, ReadRaw u) => IsUnion (IsUnionViaReadRaw u) where
  zero =
      unsafePerformIO $
      BA.withByteArrayContents zeroBytes $ \(ptr :: Ptr Word8) ->
        IsUnionViaReadRaw <$> readRaw (castPtr ptr :: Ptr u)
    where
      n = staticSizeOf (Proxy @u)
      zeroBytes = BA.byteArrayFromListN n $ replicate n (0 :: Word8)

-- | Helper instance for newtype-deriving 'IsUnion'
--
-- This instance is equivalent to the helper instance for deriving via
-- 'ReadRaw', but the former is probably more performant.
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
