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
  , "/* get___f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_7003b306f73c174b (void)) (void)"
  , "{"
  , "  return &__f1;"
  , "}"
  , "/* get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1 (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* get_my_memalign_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_b3c956e53724162c (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_memalign;"
  , "}"
  , "/* get_my_calloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_733646ca96f39979 (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_calloc;"
  , "}"
  , "/* get_my_realloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_94e8271f186110fd (void)) ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_realloc;"
  , "}"
  , "/* get_my_alloc1_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc1;"
  , "}"
  , "/* get_my_alloc2_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc2;"
  , "}"
  , "/* get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* get_old_fn_deprecated_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20 (void)) (void)"
  , "{"
  , "  return &old_fn_deprecated;"
  , "}"
  , "/* get_my_dgettext_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_test_functionsfun_attributes_a0be4f488601c252 (void)) ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &my_dgettext;"
  , "}"
  , "/* get_fdopen_ptr */"
  , "__attribute__ ((const))"
  , "FILE *(*hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326 (void)) ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &fdopen;"
  , "}"
  , "/* get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* get_my_memcpy_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8 (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* get_fatal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5 (void)) (void)"
  , "{"
  , "  return &fatal;"
  , "}"
  , "/* get_hash_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* get_mymalloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_test_functionsfun_attributes_4ce141c884649d49 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &mymalloc;"
  , "}"
  , "/* get_foobar_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa (void)) (void)"
  , "{"
  , "  return &foobar;"
  , "}"
  , "/* get_core2_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_14ef55245a14f816 (void)) (void)"
  , "{"
  , "  return &core2_func;"
  , "}"
  , "/* get_sse3_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_72956748bb6eee67 (void)) (void)"
  , "{"
  , "  return &sse3_func;"
  , "}"
  , "/* get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2 (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* get_fn_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_5929da82079150d1 (void)) (void)"
  , "{"
  , "  return &fn;"
  , "}"
  , "/* get_y_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6 (void)) (void)"
  , "{"
  , "  return &y;"
  , "}"
  , "/* get_x1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_11098262b345351a (void)) (void)"
  , "{"
  , "  return &x1;"
  , "}"
  , "/* get_x2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9 (void)) (void)"
  , "{"
  , "  return &x2;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_7003b306f73c174b" hs_bindgen_test_functionsfun_attributes_7003b306f73c174b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE __f1_ptr #-}

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1_ptr :: Ptr.FunPtr (IO ())
__f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_7003b306f73c174b

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1" hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_5469bdc0395f86c1

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_b3c956e53724162c" hs_bindgen_test_functionsfun_attributes_b3c956e53724162c ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memalign_ptr #-}

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_memalign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_b3c956e53724162c

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_733646ca96f39979" hs_bindgen_test_functionsfun_attributes_733646ca96f39979 ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_calloc_ptr #-}

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_calloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_733646ca96f39979

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_94e8271f186110fd" hs_bindgen_test_functionsfun_attributes_94e8271f186110fd ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_realloc_ptr #-}

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_realloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_94e8271f186110fd

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70" hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc1_ptr #-}

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_48d9862d70f58e70

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357" hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc2_ptr #-}

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_17a11fd10dc57357

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9" hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_c41111f40a04cdc9

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20" hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE old_fn_deprecated_ptr #-}

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated_ptr :: Ptr.FunPtr (IO FC.CInt)
old_fn_deprecated_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_17f68fdc3f464b20

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_a0be4f488601c252" hs_bindgen_test_functionsfun_attributes_a0be4f488601c252 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE my_dgettext_ptr #-}

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
my_dgettext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_a0be4f488601c252

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326" hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326 ::
     IO (Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE)))

{-# NOINLINE fdopen_ptr #-}

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen_ptr :: Ptr.FunPtr (FC.CInt -> (Ptr.Ptr FC.CChar) -> IO (Ptr.Ptr FILE))
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_2b987c3b5c01a326

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69" hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_490ca7e8c8282a69

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8" hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_e2e8b5d5ac435de8

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5" hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE fatal_ptr #-}

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal_ptr :: Ptr.FunPtr (IO ())
fatal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_ea0bb781f9eca7f5

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01" hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

{-| __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h:110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_4de9606eb9c5dd01

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_4ce141c884649d49" hs_bindgen_test_functionsfun_attributes_4ce141c884649d49 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE mymalloc_ptr #-}

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
mymalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_4ce141c884649d49

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa" hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE foobar_ptr #-}

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar_ptr :: Ptr.FunPtr (IO ())
foobar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_5c243ced544ab0aa

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_14ef55245a14f816" hs_bindgen_test_functionsfun_attributes_14ef55245a14f816 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE core2_func_ptr #-}

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func_ptr :: Ptr.FunPtr (IO FC.CInt)
core2_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_14ef55245a14f816

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_72956748bb6eee67" hs_bindgen_test_functionsfun_attributes_72956748bb6eee67 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE sse3_func_ptr #-}

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func_ptr :: Ptr.FunPtr (IO FC.CInt)
sse3_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_72956748bb6eee67

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2" hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_38506a9ac5626bf2

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_5929da82079150d1" hs_bindgen_test_functionsfun_attributes_5929da82079150d1 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE fn_ptr #-}

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn_ptr :: Ptr.FunPtr (IO FC.CInt)
fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_5929da82079150d1

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6" hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE y_ptr #-}

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y_ptr :: Ptr.FunPtr (IO FC.CInt)
y_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_7bcb4a1873e6ece6

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_11098262b345351a" hs_bindgen_test_functionsfun_attributes_11098262b345351a ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x1_ptr #-}

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1_ptr :: Ptr.FunPtr (IO FC.CInt)
x1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_11098262b345351a

foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9" hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x2_ptr #-}

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2_ptr :: Ptr.FunPtr (IO FC.CInt)
x2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_functionsfun_attributes_0d19f83087f278f9
