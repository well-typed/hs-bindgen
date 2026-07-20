{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Reuse the runtime's generated 'ToFunPtr' for a callback whose signature is
-- only 'Coercible' to a covered one.
--
-- @hs-bindgen-runtime@ generates 'ToFunPtr' (and the matching
-- @foreign import ccall "wrapper"@) for a fixed set of C signatures only, so a
-- domain-typed callback such as libclang's visitor
-- (@Ptr CXCursor_ -> Ptr CXCursor_ -> IO (SimpleEnum CXChildVisitResult)@) is not
-- covered. But it differs from a covered signature only by phantom @Ptr@
-- parameters and a newtype (@SimpleEnum@) result, so the two are 'Coercible' and
-- retagging is a zero-cost 'coerce'.
module ClangFFI.Callback (withFunPtrAs) where

import Data.Coerce (Coercible, coerce)
import Foreign.Ptr (FunPtr)

import HsBindgen.Runtime.Internal.FunPtr (ToFunPtr, withFunPtr)

-- | 'withFunPtr' for a callback whose signature is only 'Coercible' to a
-- 'ToFunPtr'-covered one. Name the covered representative with a type
-- application:
--
-- > withFunPtrAs @(Ptr Void -> Ptr Void -> IO CInt) domainTypedCallback $ \fp -> ...
--
-- The @cb@ callback is written in its natural domain types; @coerce@ retags it to
-- the covered @covered@ at no runtime cost, and the runtime's generated
-- 'ToFunPtr' does the rest. The 'FunPtr' handed back is typed at @covered@, ready
-- for a @foreign import@ declared against the covered signature.
withFunPtrAs ::
     forall covered cb r. (Coercible cb covered, ToFunPtr covered)
  => cb
  -> (FunPtr covered -> IO r)
  -> IO r
withFunPtrAs cb = withFunPtr (coerce cb :: covered)
{-# INLINE withFunPtrAs #-}
