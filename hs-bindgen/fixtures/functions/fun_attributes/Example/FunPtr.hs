{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* Example_get___f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_67d36ce76ccaa6e9 (void)) (void)"
  , "{"
  , "  return &__f1;"
  , "}"
  , "/* Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_b864c2909d300a7a (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* Example_get_my_memalign_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_8c28f5c7bf10a38d (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_memalign;"
  , "}"
  , "/* Example_get_my_calloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_d855edb5bd96667f (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_calloc;"
  , "}"
  , "/* Example_get_my_realloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_f70081ecd8b6291c (void)) ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_realloc;"
  , "}"
  , "/* Example_get_my_alloc1_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_efa603b7d9afaac2 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc1;"
  , "}"
  , "/* Example_get_my_alloc2_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_193965083040e93c (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc2;"
  , "}"
  , "/* Example_get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_61cbe351b243e6fe (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* Example_get_old_fn_deprecated_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_2d2b263a39bbc1bd (void)) (void)"
  , "{"
  , "  return &old_fn_deprecated;"
  , "}"
  , "/* Example_get_my_dgettext_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_functionsfun_attributes_aa77de4751ce490e (void)) ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &my_dgettext;"
  , "}"
  , "/* Example_get_fdopen_ptr */"
  , "__attribute__ ((const))"
  , "FILE *(*hs_bindgen_test_functionsfun_attributes_e1e4a2b3d3cabf04 (void)) ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &fdopen;"
  , "}"
  , "/* Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_66763a4fad90fe22 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* Example_get_my_memcpy_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_d3b7a85b34dd0d4a (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* Example_get_fatal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_3a9111628d070978 (void)) (void)"
  , "{"
  , "  return &fatal;"
  , "}"
  , "/* Example_get_hash_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_ad030d6582d9db0b (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* Example_get_mymalloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_39de4c2a25c2c88a (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &mymalloc;"
  , "}"
  , "/* Example_get_foobar_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_7fd08594d4628ec4 (void)) (void)"
  , "{"
  , "  return &foobar;"
  , "}"
  , "/* Example_get_core2_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_67d73dcfa38eff22 (void)) (void)"
  , "{"
  , "  return &core2_func;"
  , "}"
  , "/* Example_get_sse3_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_1e958a8cb6e4aeae (void)) (void)"
  , "{"
  , "  return &sse3_func;"
  , "}"
  , "/* Example_get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_10ff0632953957cf (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* Example_get_fn_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_1bcc3934dad3b7ee (void)) (void)"
  , "{"
  , "  return &fn;"
  , "}"
  , "/* Example_get_y_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_304e6191da749e9b (void)) (void)"
  , "{"
  , "  return &y;"
  , "}"
  , "/* Example_get_x1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_b0da0f6a0b34b7a4 (void)) (void)"
  , "{"
  , "  return &x1;"
  , "}"
  , "/* Example_get_x2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_28f54b86ce03210a (void)) (void)"
  , "{"
  , "  return &x2;"
  , "}"
  ]))

{-| __unique:__ @Example_get___f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_67d36ce76ccaa6e9" hs_bindgen_test_functionsfun_attributes_67d36ce76ccaa6e9 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE __f1_ptr #-}

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1_ptr :: Ptr.FunPtr (IO ())
__f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_67d36ce76ccaa6e9

{-| __unique:__ @Example_get_f1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_b864c2909d300a7a" hs_bindgen_test_functionsfun_attributes_b864c2909d300a7a ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_b864c2909d300a7a

{-| __unique:__ @Example_get_my_memalign_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_8c28f5c7bf10a38d" hs_bindgen_test_functionsfun_attributes_8c28f5c7bf10a38d ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memalign_ptr #-}

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_memalign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_8c28f5c7bf10a38d

{-| __unique:__ @Example_get_my_calloc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_d855edb5bd96667f" hs_bindgen_test_functionsfun_attributes_d855edb5bd96667f ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_calloc_ptr #-}

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_calloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_d855edb5bd96667f

{-| __unique:__ @Example_get_my_realloc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_f70081ecd8b6291c" hs_bindgen_test_functionsfun_attributes_f70081ecd8b6291c ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_realloc_ptr #-}

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_realloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_f70081ecd8b6291c

{-| __unique:__ @Example_get_my_alloc1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_efa603b7d9afaac2" hs_bindgen_test_functionsfun_attributes_efa603b7d9afaac2 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc1_ptr #-}

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_efa603b7d9afaac2

{-| __unique:__ @Example_get_my_alloc2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_193965083040e93c" hs_bindgen_test_functionsfun_attributes_193965083040e93c ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc2_ptr #-}

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_193965083040e93c

{-| __unique:__ @Example_get_square_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_61cbe351b243e6fe" hs_bindgen_test_functionsfun_attributes_61cbe351b243e6fe ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_61cbe351b243e6fe

{-| __unique:__ @Example_get_old_fn_deprecated_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_2d2b263a39bbc1bd" hs_bindgen_test_functionsfun_attributes_2d2b263a39bbc1bd ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE old_fn_deprecated_ptr #-}

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated_ptr :: Ptr.FunPtr (IO FC.CInt)
old_fn_deprecated_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_2d2b263a39bbc1bd

{-| __unique:__ @Example_get_my_dgettext_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_aa77de4751ce490e" hs_bindgen_test_functionsfun_attributes_aa77de4751ce490e ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE my_dgettext_ptr #-}

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
my_dgettext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_aa77de4751ce490e

{-| __unique:__ @Example_get_fdopen_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_e1e4a2b3d3cabf04" hs_bindgen_test_functionsfun_attributes_e1e4a2b3d3cabf04 ::
     IO (Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE)))

{-# NOINLINE fdopen_ptr #-}

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen_ptr :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE))
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_e1e4a2b3d3cabf04

{-| __unique:__ @Example_get_f2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_66763a4fad90fe22" hs_bindgen_test_functionsfun_attributes_66763a4fad90fe22 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_66763a4fad90fe22

{-| __unique:__ @Example_get_my_memcpy_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_d3b7a85b34dd0d4a" hs_bindgen_test_functionsfun_attributes_d3b7a85b34dd0d4a ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_d3b7a85b34dd0d4a

{-| __unique:__ @Example_get_fatal_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_3a9111628d070978" hs_bindgen_test_functionsfun_attributes_3a9111628d070978 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE fatal_ptr #-}

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal_ptr :: Ptr.FunPtr (IO ())
fatal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_3a9111628d070978

{-| __unique:__ @Example_get_hash_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_ad030d6582d9db0b" hs_bindgen_test_functionsfun_attributes_ad030d6582d9db0b ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

{-| __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h:110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_ad030d6582d9db0b

{-| __unique:__ @Example_get_mymalloc_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_39de4c2a25c2c88a" hs_bindgen_test_functionsfun_attributes_39de4c2a25c2c88a ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE mymalloc_ptr #-}

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
mymalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_39de4c2a25c2c88a

{-| __unique:__ @Example_get_foobar_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_7fd08594d4628ec4" hs_bindgen_test_functionsfun_attributes_7fd08594d4628ec4 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE foobar_ptr #-}

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar_ptr :: Ptr.FunPtr (IO ())
foobar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_7fd08594d4628ec4

{-| __unique:__ @Example_get_core2_func_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_67d73dcfa38eff22" hs_bindgen_test_functionsfun_attributes_67d73dcfa38eff22 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE core2_func_ptr #-}

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func_ptr :: Ptr.FunPtr (IO FC.CInt)
core2_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_67d73dcfa38eff22

{-| __unique:__ @Example_get_sse3_func_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_1e958a8cb6e4aeae" hs_bindgen_test_functionsfun_attributes_1e958a8cb6e4aeae ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE sse3_func_ptr #-}

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func_ptr :: Ptr.FunPtr (IO FC.CInt)
sse3_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_1e958a8cb6e4aeae

{-| __unique:__ @Example_get_f3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_10ff0632953957cf" hs_bindgen_test_functionsfun_attributes_10ff0632953957cf ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_10ff0632953957cf

{-| __unique:__ @Example_get_fn_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_1bcc3934dad3b7ee" hs_bindgen_test_functionsfun_attributes_1bcc3934dad3b7ee ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE fn_ptr #-}

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn_ptr :: Ptr.FunPtr (IO FC.CInt)
fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_1bcc3934dad3b7ee

{-| __unique:__ @Example_get_y_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_304e6191da749e9b" hs_bindgen_test_functionsfun_attributes_304e6191da749e9b ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE y_ptr #-}

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y_ptr :: Ptr.FunPtr (IO FC.CInt)
y_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_304e6191da749e9b

{-| __unique:__ @Example_get_x1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_b0da0f6a0b34b7a4" hs_bindgen_test_functionsfun_attributes_b0da0f6a0b34b7a4 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x1_ptr #-}

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1_ptr :: Ptr.FunPtr (IO FC.CInt)
x1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_b0da0f6a0b34b7a4

{-| __unique:__ @Example_get_x2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_28f54b86ce03210a" hs_bindgen_test_functionsfun_attributes_28f54b86ce03210a ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x2_ptr #-}

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2_ptr :: Ptr.FunPtr (IO FC.CInt)
x2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_28f54b86ce03210a
