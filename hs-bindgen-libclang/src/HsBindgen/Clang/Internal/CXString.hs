{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dealing with @CXString@
module HsBindgen.Clang.Internal.CXString () where

import Control.Exception
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS.Strict
import Foreign
import Foreign.C

import HsBindgen.Clang.Core.Instances ()
import HsBindgen.Clang.Core.Structs
import HsBindgen.Clang.Internal.ByValue

{-------------------------------------------------------------------------------
  Translation to bytestrings
-------------------------------------------------------------------------------}

instance Preallocate ByteString where
  type Writing ByteString = W CXString_

  preallocate :: (W CXString_ -> IO b) -> IO (ByteString, b)
  preallocate allocStr =
      bracket
          (preallocate allocStr)
          (clang_disposeString . fst) $ \(str, b) -> do
        cstr <- clang_getCString str
        if cstr == nullPtr
          then return (BS.Strict.empty, b)
          else (, b) <$> BS.Strict.packCString cstr

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
newtype CXString = CXString (OnHaskellHeap CXString_)
  deriving newtype (LivesOnHaskellHeap, Preallocate)

foreign import capi unsafe "clang_wrappers.h wrap_getCString"
  wrap_getCString :: R CXString_ -> IO CString

foreign import capi unsafe "clang_wrappers.h wrap_disposeString"
  wrap_disposeString :: R CXString_ -> IO ()

-- | Retrieve the character data associated with the given string.
--
-- We use @ccall@ rather than @capi@ here to avoid compiler warning about
-- casting @const char *@ to @char *@ (we make a copy of the C string and then
-- do not use it again, so it's safe).
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gabe1284209a3cd35c92e61a31e9459fe7>
clang_getCString :: CXString -> IO CString
clang_getCString str = onHaskellHeap str $ wrap_getCString

-- | Free the given string.
--
-- <https://clang.llvm.org/doxygen/group__CINDEX__STRING.html#gaeff715b329ded18188959fab3066048f>
clang_disposeString :: CXString -> IO ()
clang_disposeString str = onHaskellHeap str $ wrap_disposeString