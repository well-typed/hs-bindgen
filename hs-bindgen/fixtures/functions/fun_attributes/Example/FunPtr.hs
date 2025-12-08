{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* test_functionsfun_attributes_Example_get___f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_24a849cc3a4a1da5 (void)) (void)"
  , "{"
  , "  return &__f1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0e7d1a5941234285 (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_memalign_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_08646a1466ab9e1b (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_memalign;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_calloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_ed6d281e7bfe4523 (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_calloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_realloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_24c8bade35b40f21 (void)) ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_realloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_alloc1_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_930dccd393b8f937 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_alloc2_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_b15d8039514faa44 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc2;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_square_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9a26c4f7828e9f21 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_old_fn_deprecated_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_75b7d9140b40148e (void)) (void)"
  , "{"
  , "  return &old_fn_deprecated;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_dgettext_ptr */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_880fe66e7b0bf3df (void)) ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &my_dgettext;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fdopen_ptr */"
  , "__attribute__ ((const))"
  , "FILE *(*hs_bindgen_e36b210e874d5d42 (void)) ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &fdopen;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_193dba4a732d39a4 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_memcpy_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_95f5193f59c47586 (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fatal_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6944ffce3b5c6e81 (void)) (void)"
  , "{"
  , "  return &fatal;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_hash_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_71a42f1d6c853302 (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_mymalloc_ptr */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_97fdda2d31fdf3b8 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &mymalloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_foobar_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f8e2e1e043022d0b (void)) (void)"
  , "{"
  , "  return &foobar;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_core2_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c0d2203c2008c671 (void)) (void)"
  , "{"
  , "  return &core2_func;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_sse3_func_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0d9e6d9c675bc6c6 (void)) (void)"
  , "{"
  , "  return &sse3_func;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_83d5762c52905621 (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fn_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2c761cc9b4f8156d (void)) (void)"
  , "{"
  , "  return &fn;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_y_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a1175242bc62c1a1 (void)) (void)"
  , "{"
  , "  return &y;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_x1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_9ccc986739d1a164 (void)) (void)"
  , "{"
  , "  return &x1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_x2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8f44934a5928d386 (void)) (void)"
  , "{"
  , "  return &x2;"
  , "}"
  ]))

-- | __unique:__ @test_functionsfun_attributes_Example_get___f1_ptr@
foreign import ccall unsafe "hs_bindgen_24a849cc3a4a1da5" hs_bindgen_24a849cc3a4a1da5 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE __f1_ptr #-}

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1_ptr :: Ptr.FunPtr (IO ())
__f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_24a849cc3a4a1da5

-- | __unique:__ @test_functionsfun_attributes_Example_get_f1_ptr@
foreign import ccall unsafe "hs_bindgen_0e7d1a5941234285" hs_bindgen_0e7d1a5941234285 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f1_ptr #-}

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1_ptr :: Ptr.FunPtr (IO ())
f1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0e7d1a5941234285

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_memalign_ptr@
foreign import ccall unsafe "hs_bindgen_08646a1466ab9e1b" hs_bindgen_08646a1466ab9e1b ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memalign_ptr #-}

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_memalign_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_08646a1466ab9e1b

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_calloc_ptr@
foreign import ccall unsafe "hs_bindgen_ed6d281e7bfe4523" hs_bindgen_ed6d281e7bfe4523 ::
     IO (Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_calloc_ptr #-}

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc_ptr :: Ptr.FunPtr (Size_t -> Size_t -> IO (Ptr.Ptr Void))
my_calloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ed6d281e7bfe4523

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_realloc_ptr@
foreign import ccall unsafe "hs_bindgen_24c8bade35b40f21" hs_bindgen_24c8bade35b40f21 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_realloc_ptr #-}

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_realloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_24c8bade35b40f21

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_alloc1_ptr@
foreign import ccall unsafe "hs_bindgen_930dccd393b8f937" hs_bindgen_930dccd393b8f937 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc1_ptr #-}

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_930dccd393b8f937

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_alloc2_ptr@
foreign import ccall unsafe "hs_bindgen_b15d8039514faa44" hs_bindgen_b15d8039514faa44 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_alloc2_ptr #-}

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
my_alloc2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b15d8039514faa44

-- | __unique:__ @test_functionsfun_attributes_Example_get_square_ptr@
foreign import ccall unsafe "hs_bindgen_9a26c4f7828e9f21" hs_bindgen_9a26c4f7828e9f21 ::
     IO (Ptr.FunPtr (FC.CInt -> IO FC.CInt))

{-# NOINLINE square_ptr #-}

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square_ptr :: Ptr.FunPtr (FC.CInt -> IO FC.CInt)
square_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9a26c4f7828e9f21

-- | __unique:__ @test_functionsfun_attributes_Example_get_old_fn_deprecated_ptr@
foreign import ccall unsafe "hs_bindgen_75b7d9140b40148e" hs_bindgen_75b7d9140b40148e ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE old_fn_deprecated_ptr #-}

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated_ptr :: Ptr.FunPtr (IO FC.CInt)
old_fn_deprecated_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_75b7d9140b40148e

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_dgettext_ptr@
foreign import ccall unsafe "hs_bindgen_880fe66e7b0bf3df" hs_bindgen_880fe66e7b0bf3df ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar) -> IO (Ptr.Ptr FC.CChar)))

{-# NOINLINE my_dgettext_ptr #-}

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar) -> IO (Ptr.Ptr FC.CChar))
my_dgettext_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_880fe66e7b0bf3df

-- | __unique:__ @test_functionsfun_attributes_Example_get_fdopen_ptr@
foreign import ccall unsafe "hs_bindgen_e36b210e874d5d42" hs_bindgen_e36b210e874d5d42 ::
     IO (Ptr.FunPtr (FC.CInt -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar) -> IO (Ptr.Ptr FILE)))

{-# NOINLINE fdopen_ptr #-}

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen_ptr :: Ptr.FunPtr (FC.CInt -> (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar) -> IO (Ptr.Ptr FILE))
fdopen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e36b210e874d5d42

-- | __unique:__ @test_functionsfun_attributes_Example_get_f2_ptr@
foreign import ccall unsafe "hs_bindgen_193dba4a732d39a4" hs_bindgen_193dba4a732d39a4 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f2_ptr #-}

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2_ptr :: Ptr.FunPtr (IO ())
f2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_193dba4a732d39a4

-- | __unique:__ @test_functionsfun_attributes_Example_get_my_memcpy_ptr@
foreign import ccall unsafe "hs_bindgen_95f5193f59c47586" hs_bindgen_95f5193f59c47586 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Void) -> (HsBindgen.Runtime.ConstPtr.ConstPtr Void) -> Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE my_memcpy_ptr #-}

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy_ptr :: Ptr.FunPtr ((Ptr.Ptr Void) -> (HsBindgen.Runtime.ConstPtr.ConstPtr Void) -> Size_t -> IO (Ptr.Ptr Void))
my_memcpy_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_95f5193f59c47586

-- | __unique:__ @test_functionsfun_attributes_Example_get_fatal_ptr@
foreign import ccall unsafe "hs_bindgen_6944ffce3b5c6e81" hs_bindgen_6944ffce3b5c6e81 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE fatal_ptr #-}

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal_ptr :: Ptr.FunPtr (IO ())
fatal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6944ffce3b5c6e81

-- | __unique:__ @test_functionsfun_attributes_Example_get_hash_ptr@
foreign import ccall unsafe "hs_bindgen_71a42f1d6c853302" hs_bindgen_71a42f1d6c853302 ::
     IO (Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt))

{-# NOINLINE hash_ptr #-}

{-| __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h:110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash_ptr :: Ptr.FunPtr ((Ptr.Ptr FC.CChar) -> IO FC.CInt)
hash_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_71a42f1d6c853302

-- | __unique:__ @test_functionsfun_attributes_Example_get_mymalloc_ptr@
foreign import ccall unsafe "hs_bindgen_97fdda2d31fdf3b8" hs_bindgen_97fdda2d31fdf3b8 ::
     IO (Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void)))

{-# NOINLINE mymalloc_ptr #-}

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc_ptr :: Ptr.FunPtr (Size_t -> IO (Ptr.Ptr Void))
mymalloc_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_97fdda2d31fdf3b8

-- | __unique:__ @test_functionsfun_attributes_Example_get_foobar_ptr@
foreign import ccall unsafe "hs_bindgen_f8e2e1e043022d0b" hs_bindgen_f8e2e1e043022d0b ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE foobar_ptr #-}

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar_ptr :: Ptr.FunPtr (IO ())
foobar_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f8e2e1e043022d0b

-- | __unique:__ @test_functionsfun_attributes_Example_get_core2_func_ptr@
foreign import ccall unsafe "hs_bindgen_c0d2203c2008c671" hs_bindgen_c0d2203c2008c671 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE core2_func_ptr #-}

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func_ptr :: Ptr.FunPtr (IO FC.CInt)
core2_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c0d2203c2008c671

-- | __unique:__ @test_functionsfun_attributes_Example_get_sse3_func_ptr@
foreign import ccall unsafe "hs_bindgen_0d9e6d9c675bc6c6" hs_bindgen_0d9e6d9c675bc6c6 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE sse3_func_ptr #-}

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func_ptr :: Ptr.FunPtr (IO FC.CInt)
sse3_func_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0d9e6d9c675bc6c6

-- | __unique:__ @test_functionsfun_attributes_Example_get_f3_ptr@
foreign import ccall unsafe "hs_bindgen_83d5762c52905621" hs_bindgen_83d5762c52905621 ::
     IO (Ptr.FunPtr (IO ()))

{-# NOINLINE f3_ptr #-}

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3_ptr :: Ptr.FunPtr (IO ())
f3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_83d5762c52905621

-- | __unique:__ @test_functionsfun_attributes_Example_get_fn_ptr@
foreign import ccall unsafe "hs_bindgen_2c761cc9b4f8156d" hs_bindgen_2c761cc9b4f8156d ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE fn_ptr #-}

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn_ptr :: Ptr.FunPtr (IO FC.CInt)
fn_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2c761cc9b4f8156d

-- | __unique:__ @test_functionsfun_attributes_Example_get_y_ptr@
foreign import ccall unsafe "hs_bindgen_a1175242bc62c1a1" hs_bindgen_a1175242bc62c1a1 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE y_ptr #-}

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y_ptr :: Ptr.FunPtr (IO FC.CInt)
y_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a1175242bc62c1a1

-- | __unique:__ @test_functionsfun_attributes_Example_get_x1_ptr@
foreign import ccall unsafe "hs_bindgen_9ccc986739d1a164" hs_bindgen_9ccc986739d1a164 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x1_ptr #-}

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1_ptr :: Ptr.FunPtr (IO FC.CInt)
x1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9ccc986739d1a164

-- | __unique:__ @test_functionsfun_attributes_Example_get_x2_ptr@
foreign import ccall unsafe "hs_bindgen_8f44934a5928d386" hs_bindgen_8f44934a5928d386 ::
     IO (Ptr.FunPtr (IO FC.CInt))

{-# NOINLINE x2_ptr #-}

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2_ptr :: Ptr.FunPtr (IO FC.CInt)
x2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8f44934a5928d386
