-- | Ready-made marshallers for common Haskell/C type pairs, for use with @input@
-- and @output@ from "HsBindgen.Runtime.HighLevel".
--
-- Each is a thin convenience over a constructor from
-- "HsBindgen.Runtime.HighLevel.Marshaller": 'bracket' (single C argument), the
-- 'Marshal' constructor (several), or 'unmarshalOutWith'. For a conversion not covered here,
-- build your own the same way.
--
module HsBindgen.Runtime.HighLevel.Marshaller.Utils (
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

import HsBindgen.Runtime.HighLevel.Marshaller (Marshal (..), Unmarshaller,
                                               bracket, unmarshalOutWith)
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
withCStringIn :: Marshal String (PtrConst CChar -> lo') lo'
withCStringIn = bracket (\s k -> withCString s (k . PtrConst.unsafeFromPtr))
{-# INLINE withCStringIn #-}

-- | Marshal a 'ByteString' as a @(const char *, size_t)@ pair: two C arguments
-- from one Haskell value, so it uses the 'Marshal' constructor, not 'bracket'.
--
useAsByteStringLenIn
  :: Marshal ByteString (PtrConst CChar -> CSize -> lo') lo'
useAsByteStringLenIn = Marshal $ \bs lo k ->
  BS.useAsCStringLen bs $ \(p, n) ->
    k (lo (PtrConst.unsafeFromPtr p) (fromIntegral n))
{-# INLINE useAsByteStringLenIn #-}

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

-- | Allocate a fixed-size array buffer, run the call, peek the buffer back
-- as an 'IncompleteArray'.
--
peekIncompleteArrayOut
  :: Storable a => Int -> Unmarshaller (Ptr a) (IncompleteArray a)
peekIncompleteArrayOut n = unmarshalOutWith (allocaArray n) (\p -> IA.peekArray n (IA.toPtr p))
{-# INLINE peekIncompleteArrayOut #-}
