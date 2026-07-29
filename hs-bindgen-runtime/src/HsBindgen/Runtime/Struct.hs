{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Class for C structs
--
-- This module is intended to be imported qualified.
--
-- > import HsBindgen.Runtime.Prelude
-- > import HsBindgen.Runtime.Struct qualified as Struct
module HsBindgen.Runtime.Struct (
    IsStruct (..)
  , IsStructViaReadRaw (..)
  ) where

import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign.Ptr (Ptr, castPtr)
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Runtime.Marshal (ReadRaw (..), StaticSize (staticSizeOf))

class IsStruct s where
  -- | A 'zero' struct value is a struct value that is read from a zeroed-out
  -- byte array
  zero :: s

-- | Helper type for deriving 'IsStruct' via 'ReadRaw' (and 'StaticSize')
newtype IsStructViaReadRaw s = IsStructViaReadRaw s

-- | Helper instance for deriving 'IsStruct' via 'ReadRaw' (and 'StaticSize')
instance (StaticSize s, ReadRaw s) => IsStruct (IsStructViaReadRaw s) where
  zero =
      unsafePerformIO $
      BA.withByteArrayContents zeroBytes $ \(ptr :: Ptr Word8) ->
        IsStructViaReadRaw <$> readRaw (castPtr ptr :: Ptr s)
    where
      n = staticSizeOf (Proxy @s)
      zeroBytes = BA.byteArrayFromListN n $ replicate n (0 :: Word8)
