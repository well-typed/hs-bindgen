{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import Prelude (Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, return)

$(CAPI.addCSource "#include <fun_attributes.h>\nvoid hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9 (void) { __f1(); }\nvoid hs_bindgen_test_fun_attributes_8b60d38de80093fa (void) { f1(); }\nvoid *hs_bindgen_test_fun_attributes_72e7e9398b70632a (size_t arg1, size_t arg2) { return my_memalign(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_1e1fd866f4d88373 (size_t arg1, size_t arg2) { return my_calloc(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8 (void *arg1, size_t arg2) { return my_realloc(arg1, arg2); }\nvoid *hs_bindgen_test_fun_attributes_4cb3f4400795f3dc (size_t arg1) { return my_alloc1(arg1); }\nvoid *hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45 (size_t arg1) { return my_alloc2(arg1); }\nsigned int hs_bindgen_test_fun_attributes_8effe939268709e4 (signed int arg1) { return square(arg1); }\nsigned int hs_bindgen_test_fun_attributes_1dddc7f5a16104d4 (void) { return old_fn(); }\nchar *hs_bindgen_test_fun_attributes_77f81f76a170977e (char *arg1, char *arg2) { return my_dgettext(arg1, arg2); }\nFILE *hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d (signed int arg1, char *arg2) { return fdopen(arg1, arg2); }\nvoid hs_bindgen_test_fun_attributes_4a86b0420a250963 (void) { f2(); }\nvoid *hs_bindgen_test_fun_attributes_bcbe640b60445a4f (void *arg1, void *arg2, size_t arg3) { return my_memcpy(arg1, arg2, arg3); }\nvoid hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9 (void) { fatal(); }\nsigned int hs_bindgen_test_fun_attributes_71214e4420f53a0e (char *arg1) { return hash(arg1); }\nvoid *hs_bindgen_test_fun_attributes_a71e3488215ca2b1 (size_t arg1) { return mymalloc(arg1); }\nvoid hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88 (void) { foobar(); }\nsigned int hs_bindgen_test_fun_attributes_ab8f0d32c1f84295 (void) { return core2_func(); }\nsigned int hs_bindgen_test_fun_attributes_f50d1e8063148c18 (void) { return sse3_func(); }\nvoid hs_bindgen_test_fun_attributes_1b95ce9d55223970 (void) { f3(); }\n__attribute__ ((const)) signed int *get_i_ptr (void) { return &i; } \nsigned int hs_bindgen_test_fun_attributes_43b222bddec511f3 (void) { return fn(); }\nsigned int hs_bindgen_test_fun_attributes_fd90ce98862f93f3 (void) { return y(); }\nsigned int hs_bindgen_test_fun_attributes_8dadf866461c7be6 (void) { return x1(); }\nsigned int hs_bindgen_test_fun_attributes_31759f8ffef2c6b0 (void) { return x2(); }\n")

{-| Attributes on functions

  Examples from https://gcc.gnu.org/onlinedocs/gcc/Common-Function-Attributes.html

  __from C:__ @FILE@
-}
data FILE = FILE
  {}
  deriving stock (Eq, Show)

instance F.Storable FILE where

  sizeOf = \_ -> (0 :: Int)

  alignment = \_ -> (1 :: Int)

  peek = \ptr0 -> pure FILE

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          FILE -> return ()

newtype Size_t = Size_t
  { un_Size_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall safe "hs_bindgen_test_fun_attributes_d2d46ab14aa4b1f9" __f1
  :: IO ()

foreign import ccall safe "hs_bindgen_test_fun_attributes_8b60d38de80093fa" f1
  :: IO ()

foreign import ccall safe "hs_bindgen_test_fun_attributes_72e7e9398b70632a" my_memalign
  :: Size_t
  -> Size_t
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_1e1fd866f4d88373" my_calloc
  :: Size_t
  -> Size_t
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_3c7e2f0546d7f0f8" my_realloc
  :: F.Ptr Void
  -> Size_t
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_4cb3f4400795f3dc" my_alloc1
  :: Size_t
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_e3dd92fe5b87fb45" my_alloc2
  :: Size_t
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_8effe939268709e4" square
  :: FC.CInt
  -> FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_1dddc7f5a16104d4" old_fn
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_77f81f76a170977e" my_dgettext
  :: F.Ptr FC.CChar
     {- ^ __from C:__ @my_domain@ -}
  -> F.Ptr FC.CChar
     {- ^ __from C:__ @my_format@ -}
  -> IO (F.Ptr FC.CChar)

foreign import ccall safe "hs_bindgen_test_fun_attributes_d97c2ae9c1dff04d" fdopen
  :: FC.CInt
  -> F.Ptr FC.CChar
  -> IO (F.Ptr FILE)

foreign import ccall safe "hs_bindgen_test_fun_attributes_4a86b0420a250963" f2
  :: IO ()

foreign import ccall safe "hs_bindgen_test_fun_attributes_bcbe640b60445a4f" my_memcpy
  :: F.Ptr Void
     {- ^ __from C:__ @dest@ -}
  -> F.Ptr Void
     {- ^ __from C:__ @src@ -}
  -> Size_t
     {- ^ __from C:__ @len@ -}
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_fd569d78d0ba9fd9" fatal
  :: IO ()

{-|

  Marked @__attribute((pure))__@

-}
foreign import ccall safe "hs_bindgen_test_fun_attributes_71214e4420f53a0e" hash
  :: F.Ptr FC.CChar
  -> IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_a71e3488215ca2b1" mymalloc
  :: Size_t
     {- ^ __from C:__ @len@ -}
  -> IO (F.Ptr Void)

foreign import ccall safe "hs_bindgen_test_fun_attributes_f2d6c9a4f06efd88" foobar
  :: IO ()

foreign import ccall safe "hs_bindgen_test_fun_attributes_ab8f0d32c1f84295" core2_func
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_f50d1e8063148c18" sse3_func
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_1b95ce9d55223970" f3
  :: IO ()

foreign import ccall safe "get_i_ptr" i_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_43b222bddec511f3" fn
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_fd90ce98862f93f3" y
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_8dadf866461c7be6" x1
  :: IO FC.CInt

foreign import ccall safe "hs_bindgen_test_fun_attributes_31759f8ffef2c6b0" x2
  :: IO FC.CInt
