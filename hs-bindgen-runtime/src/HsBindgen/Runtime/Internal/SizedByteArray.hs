{-# OPTIONS_HADDOCK hide #-}

module HsBindgen.Runtime.Internal.SizedByteArray (
    SizedByteArray(..)
  , zeroUnionValue
) where

import Data.Coerce (Coercible, coerce)
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

-- | t'SizedByteArray's provide deriving-via support for t'ByteArray'.
--
-- Intended usage:
--
-- > newtype Foo = Foo ByteArray
-- >   deriving (Storable, Prim) via SizedByteArray 16 4
--
-- == Size
--
-- In this example, the v'ByteArray' must have size 16.
--
-- == Storable
--
-- The derived 'Storable' instance does /not/ declare that the t'ByteArray'
-- /itself/ is memory aligned in any way (indeed, the t'ByteArray' may well not
-- be pinned). It merely states that if we use 'Storable' to pass the
-- t'ByteArray' to a C function, we must
--
-- * allocate a temporary buffer (typically using 'Foreign.alloca')
-- * copy the v'ByteArray' into that buffer
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

-- | Write a t'SizedByteArray' to the specified location, which must have the
-- correct alignment (matching @m@)
instance GHC.KnownNat n => WriteRaw (SizedByteArray n m) where
  writeRaw ptrSBA (SizedByteArray arr) = do
    let ptr  = castPtr ptrSBA :: Ptr Word8
        size = fromIntegral $ GHC.natVal (Proxy @n)
    BA.copyByteArrayToAddr ptr arr 0 size

-- | Create a value of a C union with all bytes initialized to zero.
zeroUnionValue :: forall a. (Coercible a ByteArray, StaticSize a) => a
zeroUnionValue = coerce $ BA.byteArrayFromListN n $ replicate n (0 :: Word8)
  where
    n = staticSizeOf (Proxy :: Proxy a)
