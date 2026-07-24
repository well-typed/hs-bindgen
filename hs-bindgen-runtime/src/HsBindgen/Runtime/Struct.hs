{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Class for C structs
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Struct qualified as Struct
module HsBindgen.Runtime.Struct (
    IsStruct (..)
  , IsStructViaStorable (..)
  , get
  , set
  ) where

import Data.Primitive.ByteArray qualified as BA
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable (peek, sizeOf))
import GHC.Records.Compat qualified as Compat
import System.IO.Unsafe (unsafePerformIO)

class IsStruct s where
  -- | A 'zero' struct value has all the bytes of its fields set to 0
  --
  -- If the struct has nested struct or union values, then all the bytes of
  -- their fields are also set to 0.
  --
  -- The following equivalence should hold:
  --
  -- * A 'zero' struct value is equal to a struct value that is 'peek'ed from a
  --   zeroed-out byte array
  zero :: s

-- | Helper type for deriving 'IsStruct' via 'Storable'
newtype IsStructViaStorable s = IsStructViaStorable s

-- | Helper instance for deriving 'IsStruct' via 'Storable'
instance Storable s => IsStruct (IsStructViaStorable s) where
  zero =
      unsafePerformIO $
      BA.withByteArrayContents zeroBytes $ \(ptr :: Ptr Word8) ->
        IsStructViaStorable <$> peek (castPtr ptr :: Ptr s)
    where
      n = sizeOf (undefined :: s)
      zeroBytes = BA.byteArrayFromListN n $ replicate n (0 :: Word8)

get ::
     forall field struct a. Compat.HasField field struct a
  => struct
  -> a
get = Compat.getField @field

set ::
     forall field struct a. (Compat.HasField field struct a, IsStruct struct)
  => a
  -> struct
set = Compat.setField @field zero
