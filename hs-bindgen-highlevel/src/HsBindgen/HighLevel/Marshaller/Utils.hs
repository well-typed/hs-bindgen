-- | Ready-made marshallers for common Haskell/C type pairs, for use with @input@
-- and @output@ from "HsBindgen.HighLevel".
--
-- Each is a thin convenience over a constructor from
-- "HsBindgen.HighLevel.Marshaller": 'bracket' (single C argument), the
-- 'Marshal' constructor (several), or 'unmarshalOutWith'. For a conversion not covered here,
-- build your own the same way.
--
module HsBindgen.HighLevel.Marshaller.Utils (
    -- * Input marshallers
    withCStringIn
  , withCStringMutIn
  , withCStringArrayIn
  , useAsByteStringLenIn
  , constByteStringLenIn
  , unsafeByteStringIn
  , unsafeByteStringLenIn
  , withConstIncompleteArrayIn
  , funPtrIn
  , funPtrInAs
    -- * Output marshallers
  , peekCStringOut
  , byteStringOut
  , peekIncompleteArrayOut
    -- * Managed-handle output
  , outForeignPtr
    -- * Constant arguments
  , nullConst
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Coerce (Coercible)
import Foreign.C.String (CStringLen, peekCString, withCString)
import Foreign.C.Types (CChar, CSize)
import Foreign.ForeignPtr (FinalizerPtr, ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray, withArray)
import Foreign.Marshal.Utils (withMany)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable)

import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support.FunPtr (ToFunPtr, withFunPtr, withFunPtrAs)

import HsBindgen.HighLevel.Marshaller (Marshal (..), Unmarshaller, bracket,
                                       unmarshalOut, unmarshalOutWith)

{-------------------------------------------------------------------------------
  Input marshallers
-------------------------------------------------------------------------------}

-- | Marshal a 'String' as a NUL-terminated @const char *@ via 'withCString'
-- (one C argument). C must not retain the pointer past the call.
--
withCStringIn :: Marshal String (PtrConst CChar -> lo') lo'
withCStringIn = bracket (\s k -> withCString s (k . PtrConst.unsafeFromPtr))
{-# INLINE withCStringIn #-}

-- | Marshal a 'String' as a NUL-terminated non-@const@ @char *@ (one C argument),
-- the mutable-pointer counterpart of 'withCStringIn' for a C argument typed
-- @char *@. C must not retain the pointer past the call. (For 'Data.Text.Text', use
-- @'HsBindgen.HighLevel.Marshaller.at' Data.Text.unpack withCStringMutIn@.)
--
withCStringMutIn :: Marshal String (Ptr CChar -> lo') lo'
withCStringMutIn = bracket withCString
{-# INLINE withCStringMutIn #-}

-- | Marshal a @['String']@ as a NUL-terminated @const char *const *@ (one C
-- argument): each element is laid out with 'withCString' and the pointers gathered
-- into a C array. C must not retain the array or its strings past the call. Fits an
-- @argv@ \/ @envp@ parameter; its length is a separate argument (a 'fixed' at the
-- call site, or another input).
--
withCStringArrayIn :: Marshal [String] (PtrConst (PtrConst CChar) -> lo') lo'
withCStringArrayIn = Marshal $ \ss lo k ->
  withMany withCString ss $ \cstrs ->
    withArray cstrs $ \arr ->
      k (lo (PtrConst.unsafeFromPtr (castPtr arr)))
{-# INLINE withCStringArrayIn #-}

-- | Marshal a 'ByteString' as a @(const char *, size_t)@ pair, the canonical
-- byte-buffer default. It is the @CChar@ \/ @CSize@ specialization of
-- 'constByteStringLenIn'.
--
useAsByteStringLenIn
  :: Marshal ByteString (PtrConst CChar -> CSize -> lo') lo'
useAsByteStringLenIn = constByteStringLenIn
{-# INLINE useAsByteStringLenIn #-}

-- Shared plumbing for the @(const T *, len)@ ByteString marshallers: retag the
-- borrowed or copied buffer pointer and coerce the length. The @use@ bracket is
-- 'BS.useAsCStringLen' (copy and NUL-terminate) or 'BSU.unsafeUseAsCStringLen'
-- (zero-copy).
byteStringLenInWith
  :: Integral len
  => (forall r. ByteString -> (CStringLen -> IO r) -> IO r)
  -> Marshal ByteString (PtrConst a -> len -> lo') lo'
byteStringLenInWith use = Marshal $ \bs lo k ->
  use bs $ \(p, n) ->
    k (lo (PtrConst.unsafeFromPtr (castPtr p)) (fromIntegral n))
{-# INLINE byteStringLenInWith #-}

-- | Marshal a 'ByteString' as a @(const T *, len)@ pair for any pointer element type
-- @a@ and any integral length type @len@, e.g. libsodium's
-- @(const unsigned char *, unsigned long long)@. @auto@ resolves any such pair via
-- the 'HsBindgen.HighLevel.Defaults.DefaultIn' 'ByteString' default; use this
-- directly to write a marshaller by hand.
--
constByteStringLenIn
  :: Integral len => Marshal ByteString (PtrConst a -> len -> lo') lo'
constByteStringLenIn = byteStringLenInWith BS.useAsCStringLen
{-# INLINE constByteStringLenIn #-}

-- | Marshal a 'ByteString' as a bare @const T *@ (one C argument), zero-copy: the
-- pointer is the ByteString's own buffer, valid only for the call and NOT
-- NUL-terminated, so C must read a length it already knows (a fixed-size buffer) and
-- must not retain the pointer. The element type is left polymorphic. For a callee
-- that also takes the length use 'unsafeByteStringLenIn'; for a safe NUL-terminated
-- copy use 'constByteStringLenIn'.
--
unsafeByteStringIn :: Marshal ByteString (PtrConst a -> lo') lo'
unsafeByteStringIn = bracket $ \bs k ->
  BSU.unsafeUseAsCStringLen bs $ \(p, _len) ->
    k (PtrConst.unsafeFromPtr (castPtr p))
{-# INLINE unsafeByteStringIn #-}

-- | Marshal a 'ByteString' as a @(const T *, len)@ pair (@len@ any integral type),
-- zero-copy: the pointer is the ByteString's own buffer, valid only for the call and
-- NOT NUL-terminated, so C must honour the length and must not retain the pointer.
-- The zero-copy counterpart of 'constByteStringLenIn' (which copies to NUL-terminate);
-- prefer this when the callee takes an explicit length and the extra copy is unwanted.
--
unsafeByteStringLenIn
  :: Integral len => Marshal ByteString (PtrConst a -> len -> lo') lo'
unsafeByteStringLenIn = byteStringLenInWith BSU.unsafeUseAsCStringLen
{-# INLINE unsafeByteStringLenIn #-}

-- | View an 'IncompleteArray' as a read-only @const T *@ (one C argument).
--
withConstIncompleteArrayIn
  :: Storable a => Marshal (IncompleteArray a) (PtrConst a -> lo') lo'
withConstIncompleteArrayIn =
  bracket (\arr k -> IsA.withElemPtr arr (k . PtrConst.unsafeFromPtr))
{-# INLINE withConstIncompleteArrayIn #-}

-- | Pass a Haskell function as a C function pointer, bracketed via 'withFunPtr'
-- (one C argument). The 'FunPtr' is freed when the call returns, so this is only
-- safe for callbacks invoked /during/ the call.
--
funPtrIn :: ToFunPtr a => Marshal a (FunPtr a -> lo') lo'
funPtrIn = bracket withFunPtr
{-# INLINE funPtrIn #-}

-- | 'funPtrIn' for a callback whose own domain signature is not covered by a
-- 'ToFunPtr' instance but is 'Coercible' to a covered signature @b@ (see
-- 'withFunPtrAs'): the Haskell function is retagged onto @b@, whose 'FunPtr' fills
-- the C argument, and freed when the call returns. Apply @b@ with a type
-- application. A generated binding has a per-callback 'ToFunPtr' and uses 'funPtrIn'.
--
funPtrInAs :: (Coercible a b, ToFunPtr b) => Marshal a (FunPtr b -> lo') lo'
funPtrInAs = bracket withFunPtrAs
{-# INLINE funPtrInAs #-}

{-------------------------------------------------------------------------------
  Output marshallers
-------------------------------------------------------------------------------}

-- | Allocate a @cap@-byte buffer, run the call, peek a NUL-terminated 'String'
-- from it.
--
-- The callee must NUL-terminate within @cap@ bytes. 'peekCString' scans to the
-- first NUL and reads past the buffer if there is none, so size @cap@ to the C
-- contract with the terminator included.
--
peekCStringOut :: Int -> Unmarshaller (Ptr CChar) String
peekCStringOut cap = unmarshalOutWith (allocaBytes cap) peekCString
{-# INLINE peekCStringOut #-}

-- | Allocate an @n@-byte buffer, run the call, read the buffer back as a
-- 'ByteString' of exactly @n@ bytes. Unlike 'peekCStringOut' it does not scan for a
-- NUL, so it fits a caller-sized output buffer (an encrypt's ciphertext, a
-- fixed-size digest); the callee must fill all @n@ bytes.
--
byteStringOut :: Int -> Unmarshaller (Ptr a) ByteString
byteStringOut n = unmarshalOutWith (allocaBytes n) (\p -> BS.packCStringLen (castPtr p, n))
{-# INLINE byteStringOut #-}

-- | Allocate a fixed-size array buffer, run the call, peek the buffer back
-- as an 'IncompleteArray'.
--
peekIncompleteArrayOut
  :: Storable a => Int -> Unmarshaller (Ptr a) (IncompleteArray a)
peekIncompleteArrayOut n = unmarshalOutWith (allocaArray n) (\p -> IA.peekArray n (IA.toPtr p))
{-# INLINE peekIncompleteArrayOut #-}

{-------------------------------------------------------------------------------
  Managed-handle output
-------------------------------------------------------------------------------}

-- | An out-parameter that yields a /managed/ handle. Many C APIs acquire a handle
-- through a @T **@ out-pointer paired with a @_free@ function (@int
-- thing_open(thing **out, ...)@ and @void thing_free(thing *)@); this fills the
-- out-pointer, then wraps the returned @'Ptr' T@ in a 'ForeignPtr' with
-- @finalizer@ installed, so the handle frees itself at GC and the caller never frees
-- it by hand. Wrap the raw 'ForeignPtr' in a binding's own handle @newtype@ with
-- 'fmap' \/ 'Data.Coerce.coerce'. For a Haskell (rather than C) finalizer, build the
-- 'Unmarshaller' directly over 'Foreign.Concurrent.newForeignPtr'.
--
outForeignPtr :: FinalizerPtr a -> Unmarshaller (Ptr (Ptr a)) (ForeignPtr a)
outForeignPtr finalizer = unmarshalOut (newForeignPtr finalizer)
{-# INLINE outForeignPtr #-}

{-------------------------------------------------------------------------------
  Constant arguments
-------------------------------------------------------------------------------}

-- | A NULL @const@ pointer, for an optional or absent @const T *@ C argument,
-- supplied at the call site with @'HsBindgen.HighLevel.fixed' nullConst@.
--
nullConst :: PtrConst a
nullConst = PtrConst.unsafeFromPtr nullPtr
{-# INLINE nullConst #-}
