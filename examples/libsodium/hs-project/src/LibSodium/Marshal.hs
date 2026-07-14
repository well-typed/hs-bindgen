{-# LANGUAGE RankNTypes #-}

-- | The one marshaller specific to libsodium: a 'ByteString' passed as a bare
-- @const unsigned char *@ ('bytesConstIn'). Sized output buffers, @(ptr, len)@
-- pairs, and constant arguments use the runtime marshallers directly.
module LibSodium.Marshal
  ( bytesConstIn
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe qualified as BSU
import Foreign.C.Types (CUChar)
import Foreign.Ptr (castPtr)

import HsBindgen.Runtime.HighLevel.Marshaller (Marshal, bracket)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

-- | A 'ByteString' as a @const unsigned char *@ (pointer only). libsodium's
-- fixed-size arguments (key, nonce, signature, public\/secret key) carry no
-- length: C reads a fixed number of bytes, so the caller must ensure the
-- 'ByteString' is at least that long (a short one is an out-of-bounds read, not a
-- logic error). Zero-copy: the pointer is live only for the call.
bytesConstIn :: Marshal ByteString (PtrConst.PtrConst CUChar -> lo') lo'
bytesConstIn = bracket $ \bs use ->
  BSU.unsafeUseAsCStringLen bs $ \(p, _len) ->
    use (PtrConst.unsafeFromPtr (castPtr p))
