{-# OPTIONS_HADDOCK hide #-}

-- | Utilities for dealing with 'ByteArray', 'Storable', and 'Bitfield'.
--
-- The additional copying we have to do here is a bit annoying, but in the end
-- an FFI implementation based on 'Storable' is never going to be /extremely/
-- fast, as we are effectively (de)serializing. A few additional @memcpy@
-- operations are therefore not going to be a huge difference.
--
-- We /could/ choose to use pinned bytearrays. This would avoid /some/ copying,
-- but by no means all: we'd still need one copy (instead of two) in
-- 'peekByteArray' and 'pokeByteArray', and the calls to 'peek' and 'poke' in
-- 'peekFromByteArray' and 'pokeToByteArray' will (likely) do copying of their
-- own as well.
module HsBindgen.Runtime.Support.ByteArray (
     -- * Support for defining 'Storable' instances for union types
     peekByteArray
   , pokeByteArray
     -- * Support for defining setters and getters for union types
   , setUnionPayload
   , getUnionPayload
   , setUnionPayloadBits
   , getUnionPayloadBits
   ) where

import Control.Exception
import Control.Monad.Primitive (RealWorld)
import Data.Coerce (Coercible, coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray)
import Data.Primitive.ByteArray qualified as BA
import Foreign (Ptr, Storable (peek, poke), castPtr, copyBytes, plusPtr, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

import HsBindgen.Runtime.Support.Bitfield (Bitfield)
import HsBindgen.Runtime.Support.Bitfield qualified as Bitfield

{-------------------------------------------------------------------------------
  Support for defining 'Storable' instances for union types
-------------------------------------------------------------------------------}

peekByteArray :: Int -> Ptr a -> IO ByteArray
peekByteArray n src = do
    pinnedCopy <- BA.newPinnedByteArray n
    BA.withMutableByteArrayContents pinnedCopy $ \dest ->
      copyBytes dest (castPtr src) n
    BA.freezeByteArray pinnedCopy 0 n

pokeByteArray :: Ptr a -> ByteArray -> IO ()
pokeByteArray dest bytes = do
    pinnedCopy <- thawToPinned bytes
    BA.withMutableByteArrayContents pinnedCopy $ \src ->
      copyBytes dest (castPtr src) n
  where
    n = BA.sizeofByteArray bytes

{-------------------------------------------------------------------------------
  Support for defining setters and getters for union types
-------------------------------------------------------------------------------}

setUnionPayload :: forall payload union.
     ( Storable payload
     , Coercible union ByteArray
     )
  => payload -> union -> union
setUnionPayload x u = coerce (pokeToByteArray x (coerce u))

getUnionPayload :: forall payload union.
     ( Storable payload
     , Coercible union ByteArray
     )
  => union -> payload
getUnionPayload = peekFromByteArray . coerce

setUnionPayloadBits :: forall payload union.
     ( Bitfield payload
     , Coercible union ByteArray
     )
  => Int -> Int -> payload -> union -> union
setUnionPayloadBits bitOffset bitWidth x u =
    coerce $ pokeBitsToByteArray bitOffset bitWidth x (coerce u)

getUnionPayloadBits :: forall payload union.
     ( Bitfield payload
     , Coercible union ByteArray
     )
  => Int -> Int -> union -> payload
getUnionPayloadBits bitOffset bitWidth =
    peekBitsFromByteArray bitOffset bitWidth . coerce

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Read a 'Storable' value from a 'ByteArray'
--
-- Precondition:
--
-- > sizeOf (undefined :: a) <= sizeofByteArray bytes
--
-- It may well be the case that the ByteArray is /larger/ than the @a@ value;
-- 'peekFromByteArray' is intended to be used for reading values from otherwise
-- opaque unions (where @a@ is one such possible value), and so the bytearray
-- will be large enough to store the entire union.
peekFromByteArray :: forall a. Storable a => ByteArray -> a
peekFromByteArray bytes =
    assert (sizeOf (undefined :: a) <= BA.sizeofByteArray bytes) $
    unsafePerformIO $ do
      pinnedCopy <- thawToPinned bytes
      BA.withMutableByteArrayContents pinnedCopy $ \ptr ->
        peek (castPtr ptr)

-- | Write a 'Storable' value to a 'ByteArray'
--
-- Precondition:
--
-- > sizeOf (undefined :: a) <= sizeOfByteArray bytes
--
-- It may well be the case that the ByteArray is /larger/ than the @a@ value;
-- see also 'peekFromByteArray'.
pokeToByteArray ::
     forall a. Storable a
  => a
  -> ByteArray
  -> ByteArray
pokeToByteArray x bytes =
    assert (sizeOf (undefined :: a) <= bytesSize) $
    unsafePerformIO $ do
      pinnedCopy <- thawToPinned bytes
      BA.withMutableByteArrayContents pinnedCopy $ \ptr ->
        poke (castPtr ptr) x
      -- The copy constructed by 'freezeByteArray' is /not/ pinned.
      BA.freezeByteArray pinnedCopy 0 bytesSize
  where
    bytesSize = BA.sizeofByteArray bytes

-- | @'peekBitsFromByteArray' o w bs@ reads a range of bits of a 'Storable'
-- value from a 'ByteArray'
--
-- Preconditions:
--
-- > o >= 0
-- > w >= 1 && w <= 64
-- > w <= 'sizeOf' (undefined :: a) * 8
-- > o + w <= 'sizeofByteArray' bs * 8
--
-- It may well be the case that the ByteArray is /larger/ than the @union@ that
-- it represents; see also 'peekFromByteArray'.
peekBitsFromByteArray ::
     forall a. Bitfield a
     -- | Bit offset
  => Int
     -- | Bit width
  -> Int
  -> ByteArray
  -> a
peekBitsFromByteArray o w bs =
    unsafePerformIO $ do
      pinnedCopy <- thawToPinned bs
      BA.withMutableByteArrayContents pinnedCopy $ \ptr -> do
        let bounds = (ptrL, ptrR)
            ptrL = castPtr ptr
            ptrR = ptrL `plusPtr` BA.sizeofByteArray bs
            -- peekBitOffWidth assumes that the bit offset is in the inclusive
            -- range @\[0, 7\]@, so we move the pointer and update the bit
            -- offset accordingly
            ptr' = ptr `plusPtr` (o `div` 8)
            o'   = o - ((o `div` 8) * 8)
        Bitfield.peekBitOffWidth ptr' o' w bounds

-- | @'pokeBitsToByteArray' o w v bs@ Write a range of bits from a 'Storable'
-- value to a 'ByteArray'
--
-- Preconditions:
--
-- > o >= 0
-- > w >= 1 && w <= 64
-- > w <= 'sizeOf' v * 8
-- > o + w <= 'sizeofByteArray' bs * 8
--
-- It may well be the case that the ByteArray is /larger/ than the @union@ that
-- it represents; see also 'peekFromByteArray'.
pokeBitsToByteArray ::
     forall a. Bitfield a
     -- | Bit offset
  => Int
     -- | Bit width
  -> Int
  -> a
  -> ByteArray
  -> ByteArray
pokeBitsToByteArray o w v bs =
    unsafePerformIO $ do
      pinnedCopy <- thawToPinned bs
      BA.withMutableByteArrayContents pinnedCopy $ \ptr -> do
        let bounds = (ptrL, ptrR)
            ptrL = castPtr ptr
            ptrR = ptrL `plusPtr` bsSz
            -- pokeBitOffWidth assumes that the bit offset is in the inclusive
            -- range @\[0, 7\]@, so we move the pointer and update the bit
            -- offset accordingly
            ptr' = ptr `plusPtr` (o `div` 8)
            o'   = o - ((o `div` 8) * 8)
        Bitfield.pokeBitOffWidth ptr' o' w bounds v
      -- The copy constructed by 'freezeByteArray' is /not/ pinned.
      BA.freezeByteArray pinnedCopy 0 bsSz
  where
    bsSz = BA.sizeofByteArray bs

-- | Like 'Data.Primiteve.ByteArray.thawByteArray', but the new
-- | 'MutableByteArray' is pinned
thawToPinned :: ByteArray -> IO (MutableByteArray RealWorld)
thawToPinned src = do
    dest <- BA.newPinnedByteArray n
    BA.copyByteArray dest 0 src 0 n
    return dest
  where
    n = BA.sizeofByteArray src
