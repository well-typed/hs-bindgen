{-# OPTIONS_GHC -Wno-orphans #-}

-- | Dealing with @CXString@
module Clang.Internal.CXString () where

import Control.Exception
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign
import Foreign.C

import Clang.Internal.ByValue
import Clang.LowLevel.Core.Instances ()
import Clang.LowLevel.Core.Structs
import GHC.Ptr (Ptr(..))

{-------------------------------------------------------------------------------
  Translation to bytestrings

  TODO: <https://github.com/well-typed/hs-bindgen/issues/87>
  It seems @libclang@ exclusively uses UTF-8 internally, but would be good to
  find an authoritative reference to confirm this.

  TODO: <https://github.com/well-typed/hs-bindgen/issues/96>
  We could consider trying to deduplicate.
-------------------------------------------------------------------------------}

-- | @libclang@ uses UTF-8 internally
instance Preallocate Text where
  type Writing Text = W CXString_

  preallocate :: (W CXString_ -> IO b) -> IO (Text, b)
  preallocate allocStr =
      bracket
          (preallocate allocStr)
          (clang_disposeString . fst) $ \(str, b) -> do
        cstr@(Ptr addr) <- clang_getCString str
        if cstr == nullPtr then
          return (Text.empty, b)
        else do
          let !t = Text.unpackCString# addr
          return (t, b)

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
