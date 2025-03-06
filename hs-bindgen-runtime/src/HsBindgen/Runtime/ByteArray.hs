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

import Control.Monad.Primitive (RealWorld)
import Data.Coerce (Coercible, coerce)
import Data.Primitive.ByteArray (ByteArray, MutableByteArray, copyByteArray, newPinnedByteArray, freezeByteArray, withMutableByteArrayContents, sizeofByteArray)
import Foreign (Storable (poke, peek), Ptr, castPtr, copyBytes, sizeOf)
import System.IO.Unsafe (unsafePerformIO)

{-------------------------------------------------------------------------------
  Support for defining 'Storable' instances for union types
-------------------------------------------------------------------------------}

peekByteArray :: Ptr a -> Int -> IO ByteArray
peekByteArray src n = do
    pinnedCopy <- newPinnedByteArray n
    withMutableByteArrayContents pinnedCopy $ \dest ->
      copyBytes dest (castPtr src) n
    freezeByteArray pinnedCopy 0 n

pokeByteArray :: Ptr a -> ByteArray -> IO ()
pokeByteArray dest bytes = do
    pinnedCopy <- thawPinned bytes
    withMutableByteArrayContents pinnedCopy $ \src ->
      copyBytes dest (castPtr src) n
  where
    n = sizeofByteArray bytes

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

peekFromByteArray :: Storable a => ByteArray -> a
peekFromByteArray bytes = unsafePerformIO $ do
    pinnedCopy <- thawPinned bytes
    withMutableByteArrayContents pinnedCopy $ \ptr ->
      peek (castPtr ptr)

pokeToByteArray :: Storable a => Int -> a -> ByteArray
pokeToByteArray n x = unsafePerformIO $ do
    pinnedCopy <- newPinnedByteArray n
    withMutableByteArrayContents pinnedCopy $ \ptr ->
      poke (castPtr ptr) x
    freezeByteArray pinnedCopy 0 n

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

thawPinned :: ByteArray -> IO (MutableByteArray RealWorld)
thawPinned src = do
    dest <- newPinnedByteArray n
    copyByteArray dest 0 src 0 n
    return dest
  where
    n = sizeofByteArray src
