{-# LANGUAGE CApiFFI #-}

-- | Dealing with @CXString@
--
-- This is internal API; the public API deals with strict bytestrings only.
module HsBindgen.Clang.Internal.CXString (
    -- We must export the CXString constructor in order to be able to use it
    -- in FFI imports. It is not part of the public API (we don't re-export it).
    CXString(..)
  , packCXString
  ) where

import Control.Exception
import Data.ByteString qualified as BS.Strict
import Data.ByteString qualified as Strict (ByteString)
import Foreign
import Foreign.C

import HsBindgen.Clang.Internal.Bindings

{-------------------------------------------------------------------------------
  Translation to bytestrings
-------------------------------------------------------------------------------}

-- | Pack 'CXString'
--
-- This is intended to be used in a similar way to 'attachFinalizer'.
--
-- The @libclang@ functions that return a @CXString@ do so by /value/; we
-- allocate this on the heap in our wrapper functions. Since we no longer need
-- this after packing, we free the pointer after packing.
packCXString :: CXString -> IO Strict.ByteString
packCXString str =
    bracket
        (clang_getCString str)
        (\_ -> clang_disposeString str >> freePtr str) $ \cstr ->
      if cstr == nullPtr
        then return BS.Strict.empty
        else BS.Strict.packCString cstr

{-------------------------------------------------------------------------------
  Low-level bindings

  <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html>
-------------------------------------------------------------------------------}

-- | A character string.
--
-- The 'CXString' type is used to return strings from the interface when the
-- ownership of that string might differ from one call to the next. Use
-- 'clang_getCString' to retrieve the string data and, once finished with the
-- string data, call 'clang_disposeString' to free the string.
--
-- <https://clang.llvm.org/doxygen/structCXString.html>
newtype CXString = CXString (Ptr ())
  deriving newtype (IsPointer)

-- | Retrieve the character data associated with the given string.
--
-- We use @ccall@ rather than @capi@ here to avoid compiler warning about
-- casting @const char *@ to @char *@ (we make a copy of the C string and then
-- do not use it again, so it's safe).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gabe1284209a3cd35c92e61a31e9459fe7>
foreign import ccall unsafe "clang_wrappers.h wrap_getCString"
  clang_getCString :: CXString -> IO CString

-- | Free the given string.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gaeff715b329ded18188959fab3066048f>
foreign import capi unsafe "clang_wrappers.h wrap_disposeString"
  clang_disposeString :: CXString -> IO ()
