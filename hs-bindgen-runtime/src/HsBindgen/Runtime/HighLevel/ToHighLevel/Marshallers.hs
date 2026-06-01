-- | Ready-made marshallers for common Haskell/C type pairs, for use with 'input'
-- and 'output' from "HsBindgen.Runtime.HighLevel.ToHighLevel".
--
-- Each is a thin convenience over a constructor there: 'mkIn' (single C argument),
-- the 'InMarshaller' constructor (several), or 'mkOut'. A binding needing a
-- conversion not covered here builds its own the same way; this is a starting set.
--
module HsBindgen.Runtime.HighLevel.ToHighLevel.Marshallers (
    -- * Input marshallers
    withCStringIn
  , useAsByteStringLenIn
  , withConstIncompleteArrayIn
  , funPtrIn
    -- * Output marshallers
  , peekCStringOut
  , peekIncompleteArrayOut
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Foreign.C.String (peekCString, withCString)
import Foreign.C.Types (CChar, CSize)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable (Storable)

import HsBindgen.Runtime.HighLevel.ToHighLevel (InMarshaller (..),
                                                OutMarshaller, mkIn, mkOut)
import HsBindgen.Runtime.IncompleteArray (IncompleteArray)
import HsBindgen.Runtime.IncompleteArray qualified as IA
import HsBindgen.Runtime.Internal.FunPtr (ToFunPtr, withFunPtr)
import HsBindgen.Runtime.IsArray qualified as IsA
import HsBindgen.Runtime.PtrConst (PtrConst)
import HsBindgen.Runtime.PtrConst qualified as PtrConst

{-------------------------------------------------------------------------------
  Input marshallers
-------------------------------------------------------------------------------}

-- | Marshal a 'String' as a NUL-terminated @const char *@ via 'withCString'
-- (one C argument). C must not retain the pointer past the call.
--
withCStringIn :: InMarshaller (PtrConst CChar -> lo') lo' String
withCStringIn = mkIn (\s k -> withCString s (k . PtrConst.unsafeFromPtr))
{-# INLINE withCStringIn #-}

-- | Marshal a 'ByteString' as a @(const char *, size_t)@ pair: two C arguments
-- from one Haskell value, so it uses the 'InMarshaller' constructor, not 'mkIn'.
--
useAsByteStringLenIn
  :: InMarshaller (PtrConst CChar -> CSize -> lo') lo' ByteString
useAsByteStringLenIn = InMarshaller $ \bs lo k ->
  BS.useAsCStringLen bs $ \(p, n) ->
    k (lo (PtrConst.unsafeFromPtr p) (fromIntegral n))
{-# INLINE useAsByteStringLenIn #-}

-- | View an 'IncompleteArray' as a read-only @const T *@ (one C argument).
--
withConstIncompleteArrayIn
  :: Storable a => InMarshaller (PtrConst a -> lo') lo' (IncompleteArray a)
withConstIncompleteArrayIn =
  mkIn (\arr k -> IsA.withElemPtr arr (k . PtrConst.unsafeFromPtr))
{-# INLINE withConstIncompleteArrayIn #-}

-- | Pass a Haskell function as a C function pointer, bracketed via 'withFunPtr'
-- (one C argument). The 'FunPtr' is freed when the call returns, so this is only
-- safe for callbacks invoked /during/ the call.
--
funPtrIn :: ToFunPtr a => InMarshaller (FunPtr a -> lo') lo' a
funPtrIn = mkIn withFunPtr
{-# INLINE funPtrIn #-}

{-------------------------------------------------------------------------------
  Output marshallers
-------------------------------------------------------------------------------}

-- | Allocate a fixed-size byte buffer, run the call, peek a NUL-terminated
-- 'String' from it.
--
peekCStringOut :: Int -> OutMarshaller (Ptr CChar) String
peekCStringOut cap = mkOut (allocaBytes cap) peekCString
{-# INLINE peekCStringOut #-}

-- | Allocate a fixed-size array buffer, run the call, peek the buffer back
-- as an 'IncompleteArray'.
--
peekIncompleteArrayOut
  :: Storable a => Int -> OutMarshaller (Ptr a) (IncompleteArray a)
peekIncompleteArrayOut n = mkOut (allocaArray n) (\p -> IA.peekArray n (IA.toPtr p))
{-# INLINE peekIncompleteArrayOut #-}
