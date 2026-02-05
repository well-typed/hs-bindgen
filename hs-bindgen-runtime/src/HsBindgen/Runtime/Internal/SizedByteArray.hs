{-# OPTIONS_HADDOCK hide #-}

module HsBindgen.Runtime.Internal.SizedByteArray (
    SizedByteArray (..),
) where

import Data.Primitive.ByteArray (ByteArray (..))
import Data.Primitive.ByteArray qualified as BA
import Data.Proxy (Proxy (..))
import Data.Word (Word8)
import Foreign (Storable (..))
import Foreign.Ptr (Ptr, castPtr)
import GHC.TypeNats qualified as GHC

import HsBindgen.Runtime.Marshal

{-------------------------------------------------------------------------------
  Definition
-------------------------------------------------------------------------------}

-- | 'SizedByteArray's provide deriving-via support for 'ByteArray'.
--
-- Intended usage:
--
-- > newtype Foo = Foo ByteArray
-- >   deriving (Storable, Prim) via SizedByteArray 16 4
--
-- == Size
--
-- In this example, the 'ByteArray' must have size 16.
--
-- == Storable
--
-- The derived 'Storable' instance does /not/ declare that the 'ByteArray'
-- /itself/ is memory aligned in any way (indeed, the 'ByteArray' may well not
-- be pinned). It merely states that if we use 'Storable' to pass the
-- 'ByteArray' to a C function, we must
--
-- * allocate a temporary buffer (typically using 'Foreign.alloca')
-- * copy the 'ByteArray' into that buffer
-- * call the C function, passing a pointer to this buffer
--
-- where /that temporary buffer/ must be memory aligned.
newtype SizedByteArray (size :: GHC.Nat) (alignment :: GHC.Nat) =
    SizedByteArray ByteArray
  deriving Storable via EquivStorable (SizedByteArray size alignment)

{-------------------------------------------------------------------------------
  StaticSize, ReadRaw, WriteRaw
-------------------------------------------------------------------------------}

instance (GHC.KnownNat n, GHC.KnownNat m) => StaticSize (SizedByteArray n m) where
  staticSizeOf    _ = fromIntegral (GHC.natVal (Proxy @n))
  staticAlignment _ = fromIntegral (GHC.natVal (Proxy @m))

instance GHC.KnownNat n => ReadRaw (SizedByteArray n m) where
  readRaw ptrSBA = do
    let ptr  = castPtr ptrSBA :: Ptr Word8
        size = fromIntegral $ GHC.natVal (Proxy @n)
    arr <- BA.newByteArray size
    BA.copyPtrToMutableByteArray arr 0 ptr size
    SizedByteArray <$> BA.unsafeFreezeByteArray arr

-- | Write a 'SizedByteArray' to the specified location, which must have the
-- correct alignment (matching @m@)
instance GHC.KnownNat n => WriteRaw (SizedByteArray n m) where
  writeRaw ptrSBA (SizedByteArray arr) = do
    let ptr  = castPtr ptrSBA :: Ptr Word8
        size = fromIntegral $ GHC.natVal (Proxy @n)
    BA.copyByteArrayToAddr ptr arr 0 size
