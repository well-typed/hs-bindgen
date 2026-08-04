{-# OPTIONS_HADDOCK hide #-}

-- | Utilities for dealing with 'ByteArray' and 'Storable'
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
   ) where

import Control.Exception
import Control.Monad.Primitive (RealWorld)
import Data.Coerce (Coercible, coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray)
import Data.Primitive.ByteArray qualified as BA
import Foreign (Ptr, Storable (peek, poke), castPtr, copyBytes, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

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

-- | Like 'Data.Primiteve.ByteArray.thawByteArray', but the new
-- | 'MutableByteArray' is pinned
thawToPinned :: ByteArray -> IO (MutableByteArray RealWorld)
thawToPinned src = do
    dest <- BA.newPinnedByteArray n
    BA.copyByteArray dest 0 src 0 n
    return dest
  where
    n = BA.sizeofByteArray src
