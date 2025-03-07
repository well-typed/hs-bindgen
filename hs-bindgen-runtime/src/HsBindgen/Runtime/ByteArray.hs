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
module HsBindgen.Runtime.ByteArray (
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
import Foreign (Storable (poke, peek), Ptr, castPtr, copyBytes, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Support for defining 'Storable' instances for union types
-------------------------------------------------------------------------------}

peekByteArray :: Ptr a -> Int -> IO ByteArray
peekByteArray src n = do
    pinnedCopy <- BA.newPinnedByteArray n
    BA.withMutableByteArrayContents pinnedCopy $ \dest ->
      copyBytes dest (castPtr src) n
    BA.freezeByteArray pinnedCopy 0 n

pokeByteArray :: Ptr a -> ByteArray -> IO ()
pokeByteArray dest bytes = do
    pinnedCopy <- thawPinned bytes
    BA.withMutableByteArrayContents pinnedCopy $ \src ->
      copyBytes dest (castPtr src) n
  where
    n = BA.sizeofByteArray bytes

{-------------------------------------------------------------------------------
  Support for defining setters and getters for union types
-------------------------------------------------------------------------------}

setUnionPayload :: forall payload union.
     ( Storable payload
     , Storable union
     , Coercible union ByteArray
     )
  => payload -> union
setUnionPayload = coerce . pokeToByteArray (sizeOf (undefined :: union))

getUnionPayload :: forall payload union.
     ( Storable payload
     , Storable union
     , Coercible union ByteArray
     )
  => union -> payload
getUnionPayload = peekFromByteArray . coerce

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

-- | Read 'Storable' value from 'ByteArray'
--
-- Precondition:
--
-- > sizeOf (undefined :: a) <= sizeofByteArray bytes
--
-- It may well be the case that the ByteArray is /larger/; 'peekFromByteArray'
-- is intended to be used for reading values from otherwise opaque unions (where
-- @a@ is one such possible value), and so the bytearray will be large enough to
-- store the entire union.
peekFromByteArray :: forall a. Storable a => ByteArray -> a
peekFromByteArray bytes =
    assert (sizeOf (undefined :: a) <= BA.sizeofByteArray bytes) $
    unsafePerformIO $ do
      pinnedCopy <- thawPinned bytes
      BA.withMutableByteArrayContents pinnedCopy $ \ptr ->
        peek (castPtr ptr)

-- | Write 'Storable' value to new 'ByteArray' of specified size
--
-- Precondition:
--
-- > sizeOf (undefined :: a) <= n
--
-- It may well be that @n@ is larger; see also 'peekFromByteArray'.
pokeToByteArray :: forall a. Storable a => Int -> a -> ByteArray
pokeToByteArray n x =
    assert (sizeOf (undefined :: a) <= n) $
    unsafePerformIO $ do
      pinnedCopy <- BA.newPinnedByteArray n
      BA.withMutableByteArrayContents pinnedCopy $ \ptr ->
        poke (castPtr ptr) x
      BA.freezeByteArray pinnedCopy 0 n

-- | Like 'thawByteArray', but the 'MutableByteArray' is pinned
thawPinned :: ByteArray -> IO (MutableByteArray RealWorld)
thawPinned src = do
    dest <- BA.newPinnedByteArray n
    BA.copyByteArray dest 0 src 0 n
    return dest
  where
    n = BA.sizeofByteArray src
