{-# LANGUAGE RankNTypes #-}

-- | Shared marshallers, and the two small combinators this example proposes for
-- the runtime.
--
-- libsodium functions take @const unsigned char *@ inputs (plus an
-- @unsigned long long@ length for variable-length ones) and write into a
-- caller-allocated @unsigned char *@ output. The runtime's ready-made
-- 'HsBindgen.Runtime.HighLevel.Marshaller.Utils.useAsByteStringLenIn' assumes
-- @(const char *, size_t)@, so it does not fit @(const unsigned char *,
-- unsigned long long)@; hence 'bytesLenConstIn' here.
--
-- 'byteStringOut' (a sized output buffer) and 'fixed' (a constant C argument) are
-- the two pieces the runtime does not provide; both are spelled out below and
-- proposed for it.
module LibSodium.Marshal
  ( bytesConstIn
  , bytesLenConstIn
  , byteStringOut
  , fixed
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CUChar, CULLong)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)

import HsBindgen.Runtime.HighLevel (ToHighLevel, scratch)
import HsBindgen.Runtime.HighLevel.Internal.Threading (ThreadIn)
import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (Marshal), Unmarshaller,
                                               bracket, unmarshalOutWith)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

-- | A 'ByteString' as a @const unsigned char *@ (pointer only). The length is a
-- documented invariant of libsodium's fixed-size arguments (key, nonce,
-- signature, public\/secret key), not a separate C argument. Zero-copy: the
-- pointer is live only for the call.
bytesConstIn :: Marshal ByteString (PtrConst.PtrConst CUChar -> lo') lo'
bytesConstIn = bracket $ \bs use ->
  BSU.unsafeUseAsCStringLen bs $ \(p, _len) ->
    use (PtrConst.unsafeFromPtr (castPtr p))

-- | A 'ByteString' as the @(const unsigned char *, unsigned long long)@ pair
-- libsodium uses for variable-length inputs (a message). Two C arguments from one
-- value, so it is dropped in with 'HsBindgen.Runtime.HighLevel.input2'. The
-- ready-made @useAsByteStringLenIn@ cannot be reused: it produces
-- @(PtrConst CChar, CSize)@, not @(PtrConst CUChar, CULLong)@.
bytesLenConstIn :: Marshal ByteString (PtrConst.PtrConst CUChar -> CULLong -> lo') lo'
bytesLenConstIn = Marshal $ \bs f k ->
  BSU.unsafeUseAsCStringLen bs $ \(p, len) ->
    k (f (PtrConst.unsafeFromPtr (castPtr p)) (fromIntegral len))

-- | Allocate an @n@-byte output buffer, run the call, and copy it back as a
-- 'ByteString'.
--
-- This is the combinator the runtime is missing. 'HsBindgen.Runtime.HighLevel'
-- has 'HsBindgen.Runtime.HighLevel.output' but the only ready-made unmarshallers
-- are for scalars ('HsBindgen.Runtime.HighLevel.Defaults.DefaultOut'),
-- NUL-terminated strings (@peekCStringOut@), and 'IncompleteArray'
-- (@peekIncompleteArrayOut@). A raw sized byte buffer, which every libsodium
-- encrypt\/sign\/hash\/keygen writes, has no ready-made piece and @auto@ cannot
-- fill it, so it is hand-rolled here with 'unmarshalOutWith'.
--
-- It is polymorphic in the pointer element, so one definition fits every output
-- libsodium presents: @Ptr CUChar@ (secretbox\/sign), @Ptr Void@
-- (@randombytes_buf@), and @Ptr (Elem (ConstantArray n CUChar))@ (@keygen@). The
-- size is the caller's responsibility because a buffer has no canonical length;
-- for libsodium it is an input length plus a constant, or a fixed key\/signature
-- size. Proposed for @hs-bindgen-runtime@ next to @peekCStringOut@.
byteStringOut :: Int -> Unmarshaller (Ptr a) ByteString
byteStringOut n = unmarshalOutWith
  (\use -> allocaBytes n (\p -> use (castPtr p)))
  (\p -> BS.packCStringLen (castPtr p, n))

-- | Supply a fixed C argument the wrapper never exposes: a constant, a @NULL@, or
-- an already-allocated buffer threaded across several calls (a multipart state).
--
-- @fixed c = 'scratch' (\\k -> k c)@. 'scratch' is the mechanism (it contributes
-- no wrapper argument and no result), but its name does not reveal that passing a
-- fixed argument is a use for it. libsodium needs this for @randombytes_buf@'s
-- size (coupled to the output length) and for the @crypto_sign_state@ pointer
-- shared by @init@\/@update@\/@final@. Proposed for the runtime.
fixed :: ThreadIn lo' hi => c -> ToHighLevel lo' hi -> ToHighLevel (c -> lo') hi
fixed c = scratch (\k -> k c)
