{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/fun_attributes.h>"
  , "/* test_functionsfun_attributes_Example_get___f1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_99da7108788a2cf4 (void)) (void)"
  , "{"
  , "  return &__f1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a0d2ec6ffc23fc5a (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_memalign */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_16a2a5a807cc26ec (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_memalign;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_calloc */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_976df97f9c9c1223 (void)) ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_calloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_realloc */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_4021d3c6eae30c8c (void)) ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return &my_realloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_alloc1 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_2ca35d0827c76ff7 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_alloc2 */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_a43746fed9206f42 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &my_alloc2;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_square */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d41557222fd2ab94 (void)) ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return &square;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_old_fn_deprecated */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_beee43878922a900 (void)) (void)"
  , "{"
  , "  return &old_fn_deprecated;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_dgettext */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_f95f43d50ed8f2b4 (void)) ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &my_dgettext;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fdopen */"
  , "__attribute__ ((const))"
  , "FILE *(*hs_bindgen_986260288574592d (void)) ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return &fdopen;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_fa90d405c15977a2 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_my_memcpy */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_481e3a75205c85f2 (void)) ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return &my_memcpy;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fatal */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_99cc62837ebec657 (void)) (void)"
  , "{"
  , "  return &fatal;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_hash */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_196cbfce2a9df94f (void)) ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return &hash;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_mymalloc */"
  , "__attribute__ ((const))"
  , "void *(*hs_bindgen_ae6b503283507bd2 (void)) ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return &mymalloc;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_foobar */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ad3e5c88477c9e92 (void)) (void)"
  , "{"
  , "  return &foobar;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_core2_func */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_508f28109ff2076e (void)) (void)"
  , "{"
  , "  return &core2_func;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_sse3_func */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_6e4f5b53b36a3ecb (void)) (void)"
  , "{"
  , "  return &sse3_func;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_f3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e39d5186f5da6c3f (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_fn */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_acb0e567f47dc0eb (void)) (void)"
  , "{"
  , "  return &fn;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_y */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a1f58949b267b87f (void)) (void)"
  , "{"
  , "  return &y;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_x1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_06bd05eb2df3a936 (void)) (void)"
  , "{"
  , "  return &x1;"
  , "}"
  , "/* test_functionsfun_attributes_Example_get_x2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f2ae2154ea32940b (void)) (void)"
  , "{"
  , "  return &x2;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_Example_get___f1@
foreign import ccall unsafe "hs_bindgen_99da7108788a2cf4" hs_bindgen_99da7108788a2cf4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get___f1@
hs_bindgen_99da7108788a2cf4 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_99da7108788a2cf4 =
  RIP.fromFFIType hs_bindgen_99da7108788a2cf4_base

{-# NOINLINE __f1 #-}
{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h 16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1 :: RIP.FunPtr (IO ())
__f1 =
  RIP.unsafePerformIO hs_bindgen_99da7108788a2cf4

-- __unique:__ @test_functionsfun_attributes_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_a0d2ec6ffc23fc5a" hs_bindgen_a0d2ec6ffc23fc5a_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f1@
hs_bindgen_a0d2ec6ffc23fc5a :: IO (RIP.FunPtr (IO ()))
hs_bindgen_a0d2ec6ffc23fc5a =
  RIP.fromFFIType hs_bindgen_a0d2ec6ffc23fc5a_base

{-# NOINLINE f1 #-}
{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h 19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: RIP.FunPtr (IO ())
f1 = RIP.unsafePerformIO hs_bindgen_a0d2ec6ffc23fc5a

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memalign@
foreign import ccall unsafe "hs_bindgen_16a2a5a807cc26ec" hs_bindgen_16a2a5a807cc26ec_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memalign@
hs_bindgen_16a2a5a807cc26ec :: IO (RIP.FunPtr (Size_t -> Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_16a2a5a807cc26ec =
  RIP.fromFFIType hs_bindgen_16a2a5a807cc26ec_base

{-# NOINLINE my_memalign #-}
{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h 23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign :: RIP.FunPtr (Size_t -> Size_t -> IO (RIP.Ptr RIP.Void))
my_memalign =
  RIP.unsafePerformIO hs_bindgen_16a2a5a807cc26ec

-- __unique:__ @test_functionsfun_attributes_Example_get_my_calloc@
foreign import ccall unsafe "hs_bindgen_976df97f9c9c1223" hs_bindgen_976df97f9c9c1223_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_calloc@
hs_bindgen_976df97f9c9c1223 :: IO (RIP.FunPtr (Size_t -> Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_976df97f9c9c1223 =
  RIP.fromFFIType hs_bindgen_976df97f9c9c1223_base

{-# NOINLINE my_calloc #-}
{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h 28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc :: RIP.FunPtr (Size_t -> Size_t -> IO (RIP.Ptr RIP.Void))
my_calloc =
  RIP.unsafePerformIO hs_bindgen_976df97f9c9c1223

-- __unique:__ @test_functionsfun_attributes_Example_get_my_realloc@
foreign import ccall unsafe "hs_bindgen_4021d3c6eae30c8c" hs_bindgen_4021d3c6eae30c8c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_realloc@
hs_bindgen_4021d3c6eae30c8c :: IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_4021d3c6eae30c8c =
  RIP.fromFFIType hs_bindgen_4021d3c6eae30c8c_base

{-# NOINLINE my_realloc #-}
{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h 29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> Size_t -> IO (RIP.Ptr RIP.Void))
my_realloc =
  RIP.unsafePerformIO hs_bindgen_4021d3c6eae30c8c

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc1@
foreign import ccall unsafe "hs_bindgen_2ca35d0827c76ff7" hs_bindgen_2ca35d0827c76ff7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc1@
hs_bindgen_2ca35d0827c76ff7 :: IO (RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_2ca35d0827c76ff7 =
  RIP.fromFFIType hs_bindgen_2ca35d0827c76ff7_base

{-# NOINLINE my_alloc1 #-}
{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h 34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 :: RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void))
my_alloc1 =
  RIP.unsafePerformIO hs_bindgen_2ca35d0827c76ff7

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc2@
foreign import ccall unsafe "hs_bindgen_a43746fed9206f42" hs_bindgen_a43746fed9206f42_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc2@
hs_bindgen_a43746fed9206f42 :: IO (RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_a43746fed9206f42 =
  RIP.fromFFIType hs_bindgen_a43746fed9206f42_base

{-# NOINLINE my_alloc2 #-}
{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h 35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 :: RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void))
my_alloc2 =
  RIP.unsafePerformIO hs_bindgen_a43746fed9206f42

-- __unique:__ @test_functionsfun_attributes_Example_get_square@
foreign import ccall unsafe "hs_bindgen_d41557222fd2ab94" hs_bindgen_d41557222fd2ab94_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_square@
hs_bindgen_d41557222fd2ab94 :: IO (RIP.FunPtr (RIP.CInt -> IO RIP.CInt))
hs_bindgen_d41557222fd2ab94 =
  RIP.fromFFIType hs_bindgen_d41557222fd2ab94_base

{-# NOINLINE square #-}
{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h 39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square :: RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
square =
  RIP.unsafePerformIO hs_bindgen_d41557222fd2ab94

-- __unique:__ @test_functionsfun_attributes_Example_get_old_fn_deprecated@
foreign import ccall unsafe "hs_bindgen_beee43878922a900" hs_bindgen_beee43878922a900_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_old_fn_deprecated@
hs_bindgen_beee43878922a900 :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_beee43878922a900 =
  RIP.fromFFIType hs_bindgen_beee43878922a900_base

{-# NOINLINE old_fn_deprecated #-}
{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h 48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: RIP.FunPtr (IO RIP.CInt)
old_fn_deprecated =
  RIP.unsafePerformIO hs_bindgen_beee43878922a900

-- __unique:__ @test_functionsfun_attributes_Example_get_my_dgettext@
foreign import ccall unsafe "hs_bindgen_f95f43d50ed8f2b4" hs_bindgen_f95f43d50ed8f2b4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_dgettext@
hs_bindgen_f95f43d50ed8f2b4 :: IO (RIP.FunPtr ((RIP.Ptr RIP.CChar) -> (PtrConst.PtrConst RIP.CChar) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_f95f43d50ed8f2b4 =
  RIP.fromFFIType hs_bindgen_f95f43d50ed8f2b4_base

{-# NOINLINE my_dgettext #-}
{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h 64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext :: RIP.FunPtr ((RIP.Ptr RIP.CChar) -> (PtrConst.PtrConst RIP.CChar) -> IO (RIP.Ptr RIP.CChar))
my_dgettext =
  RIP.unsafePerformIO hs_bindgen_f95f43d50ed8f2b4

-- __unique:__ @test_functionsfun_attributes_Example_get_fdopen@
foreign import ccall unsafe "hs_bindgen_986260288574592d" hs_bindgen_986260288574592d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fdopen@
hs_bindgen_986260288574592d :: IO (RIP.FunPtr (RIP.CInt -> (PtrConst.PtrConst RIP.CChar) -> IO (RIP.Ptr FILE)))
hs_bindgen_986260288574592d =
  RIP.fromFFIType hs_bindgen_986260288574592d_base

{-# NOINLINE fdopen #-}
{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h 75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen :: RIP.FunPtr (RIP.CInt -> (PtrConst.PtrConst RIP.CChar) -> IO (RIP.Ptr FILE))
fdopen =
  RIP.unsafePerformIO hs_bindgen_986260288574592d

-- __unique:__ @test_functionsfun_attributes_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_fa90d405c15977a2" hs_bindgen_fa90d405c15977a2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f2@
hs_bindgen_fa90d405c15977a2 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_fa90d405c15977a2 =
  RIP.fromFFIType hs_bindgen_fa90d405c15977a2_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h 79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: RIP.FunPtr (IO ())
f2 = RIP.unsafePerformIO hs_bindgen_fa90d405c15977a2

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memcpy@
foreign import ccall unsafe "hs_bindgen_481e3a75205c85f2" hs_bindgen_481e3a75205c85f2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memcpy@
hs_bindgen_481e3a75205c85f2 :: IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> (PtrConst.PtrConst RIP.Void) -> Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_481e3a75205c85f2 =
  RIP.fromFFIType hs_bindgen_481e3a75205c85f2_base

{-# NOINLINE my_memcpy #-}
{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h 85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy :: RIP.FunPtr ((RIP.Ptr RIP.Void) -> (PtrConst.PtrConst RIP.Void) -> Size_t -> IO (RIP.Ptr RIP.Void))
my_memcpy =
  RIP.unsafePerformIO hs_bindgen_481e3a75205c85f2

-- __unique:__ @test_functionsfun_attributes_Example_get_fatal@
foreign import ccall unsafe "hs_bindgen_99cc62837ebec657" hs_bindgen_99cc62837ebec657_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fatal@
hs_bindgen_99cc62837ebec657 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_99cc62837ebec657 =
  RIP.fromFFIType hs_bindgen_99cc62837ebec657_base

{-# NOINLINE fatal #-}
{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h 102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: RIP.FunPtr (IO ())
fatal =
  RIP.unsafePerformIO hs_bindgen_99cc62837ebec657

-- __unique:__ @test_functionsfun_attributes_Example_get_hash@
foreign import ccall unsafe "hs_bindgen_196cbfce2a9df94f" hs_bindgen_196cbfce2a9df94f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_hash@
hs_bindgen_196cbfce2a9df94f :: IO (RIP.FunPtr ((RIP.Ptr RIP.CChar) -> IO RIP.CInt))
hs_bindgen_196cbfce2a9df94f =
  RIP.fromFFIType hs_bindgen_196cbfce2a9df94f_base

{-# NOINLINE hash #-}
{-| __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h 110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash :: RIP.FunPtr ((RIP.Ptr RIP.CChar) -> IO RIP.CInt)
hash =
  RIP.unsafePerformIO hs_bindgen_196cbfce2a9df94f

-- __unique:__ @test_functionsfun_attributes_Example_get_mymalloc@
foreign import ccall unsafe "hs_bindgen_ae6b503283507bd2" hs_bindgen_ae6b503283507bd2_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_mymalloc@
hs_bindgen_ae6b503283507bd2 :: IO (RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void)))
hs_bindgen_ae6b503283507bd2 =
  RIP.fromFFIType hs_bindgen_ae6b503283507bd2_base

{-# NOINLINE mymalloc #-}
{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h 115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc :: RIP.FunPtr (Size_t -> IO (RIP.Ptr RIP.Void))
mymalloc =
  RIP.unsafePerformIO hs_bindgen_ae6b503283507bd2

-- __unique:__ @test_functionsfun_attributes_Example_get_foobar@
foreign import ccall unsafe "hs_bindgen_ad3e5c88477c9e92" hs_bindgen_ad3e5c88477c9e92_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_foobar@
hs_bindgen_ad3e5c88477c9e92 :: IO (RIP.FunPtr (IO ()))
hs_bindgen_ad3e5c88477c9e92 =
  RIP.fromFFIType hs_bindgen_ad3e5c88477c9e92_base

{-# NOINLINE foobar #-}
{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h 119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: RIP.FunPtr (IO ())
foobar =
  RIP.unsafePerformIO hs_bindgen_ad3e5c88477c9e92

-- __unique:__ @test_functionsfun_attributes_Example_get_core2_func@
foreign import ccall unsafe "hs_bindgen_508f28109ff2076e" hs_bindgen_508f28109ff2076e_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_core2_func@
hs_bindgen_508f28109ff2076e :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_508f28109ff2076e =
  RIP.fromFFIType hs_bindgen_508f28109ff2076e_base

{-# NOINLINE core2_func #-}
{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h 126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: RIP.FunPtr (IO RIP.CInt)
core2_func =
  RIP.unsafePerformIO hs_bindgen_508f28109ff2076e

-- __unique:__ @test_functionsfun_attributes_Example_get_sse3_func@
foreign import ccall unsafe "hs_bindgen_6e4f5b53b36a3ecb" hs_bindgen_6e4f5b53b36a3ecb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_sse3_func@
hs_bindgen_6e4f5b53b36a3ecb :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_6e4f5b53b36a3ecb =
  RIP.fromFFIType hs_bindgen_6e4f5b53b36a3ecb_base

{-# NOINLINE sse3_func #-}
{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h 127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: RIP.FunPtr (IO RIP.CInt)
sse3_func =
  RIP.unsafePerformIO hs_bindgen_6e4f5b53b36a3ecb

-- __unique:__ @test_functionsfun_attributes_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_e39d5186f5da6c3f" hs_bindgen_e39d5186f5da6c3f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f3@
hs_bindgen_e39d5186f5da6c3f :: IO (RIP.FunPtr (IO ()))
hs_bindgen_e39d5186f5da6c3f =
  RIP.fromFFIType hs_bindgen_e39d5186f5da6c3f_base

{-# NOINLINE f3 #-}
{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h 131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: RIP.FunPtr (IO ())
f3 = RIP.unsafePerformIO hs_bindgen_e39d5186f5da6c3f

-- __unique:__ @test_functionsfun_attributes_Example_get_fn@
foreign import ccall unsafe "hs_bindgen_acb0e567f47dc0eb" hs_bindgen_acb0e567f47dc0eb_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fn@
hs_bindgen_acb0e567f47dc0eb :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_acb0e567f47dc0eb =
  RIP.fromFFIType hs_bindgen_acb0e567f47dc0eb_base

{-# NOINLINE fn #-}
{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h 136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: RIP.FunPtr (IO RIP.CInt)
fn = RIP.unsafePerformIO hs_bindgen_acb0e567f47dc0eb

-- __unique:__ @test_functionsfun_attributes_Example_get_y@
foreign import ccall unsafe "hs_bindgen_a1f58949b267b87f" hs_bindgen_a1f58949b267b87f_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_y@
hs_bindgen_a1f58949b267b87f :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_a1f58949b267b87f =
  RIP.fromFFIType hs_bindgen_a1f58949b267b87f_base

{-# NOINLINE y #-}
{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h 142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: RIP.FunPtr (IO RIP.CInt)
y = RIP.unsafePerformIO hs_bindgen_a1f58949b267b87f

-- __unique:__ @test_functionsfun_attributes_Example_get_x1@
foreign import ccall unsafe "hs_bindgen_06bd05eb2df3a936" hs_bindgen_06bd05eb2df3a936_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_x1@
hs_bindgen_06bd05eb2df3a936 :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_06bd05eb2df3a936 =
  RIP.fromFFIType hs_bindgen_06bd05eb2df3a936_base

{-# NOINLINE x1 #-}
{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h 145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: RIP.FunPtr (IO RIP.CInt)
x1 = RIP.unsafePerformIO hs_bindgen_06bd05eb2df3a936

-- __unique:__ @test_functionsfun_attributes_Example_get_x2@
foreign import ccall unsafe "hs_bindgen_f2ae2154ea32940b" hs_bindgen_f2ae2154ea32940b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_x2@
hs_bindgen_f2ae2154ea32940b :: IO (RIP.FunPtr (IO RIP.CInt))
hs_bindgen_f2ae2154ea32940b =
  RIP.fromFFIType hs_bindgen_f2ae2154ea32940b_base

{-# NOINLINE x2 #-}
{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h 148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: RIP.FunPtr (IO RIP.CInt)
x2 = RIP.unsafePerformIO hs_bindgen_f2ae2154ea32940b
