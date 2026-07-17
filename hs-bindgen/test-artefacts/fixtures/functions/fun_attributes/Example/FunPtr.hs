{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.__f1
    , Example.FunPtr.f1
    , Example.FunPtr.my_memalign
    , Example.FunPtr.my_calloc
    , Example.FunPtr.my_realloc
    , Example.FunPtr.my_alloc1
    , Example.FunPtr.my_alloc2
    , Example.FunPtr.square
    , Example.FunPtr.old_fn_deprecated
    , Example.FunPtr.my_dgettext
    , Example.FunPtr.fdopen
    , Example.FunPtr.f2
    , Example.FunPtr.my_memcpy
    , Example.FunPtr.fatal
    , Example.FunPtr.hash
    , Example.FunPtr.mymalloc
    , Example.FunPtr.foobar
    , Example.FunPtr.core2_func
    , Example.FunPtr.sse3_func
    , Example.FunPtr.f3
    , Example.FunPtr.fn
    , Example.FunPtr.y
    , Example.FunPtr.x1
    , Example.FunPtr.x2
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get___f1@
hs_bindgen_99da7108788a2cf4 :: IO (BG.FunPtr (IO ()))
hs_bindgen_99da7108788a2cf4 =
  BG.fromFFIType hs_bindgen_99da7108788a2cf4_base

{-# NOINLINE __f1 #-}
{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h 16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1 :: BG.FunPtr (IO ())
__f1 = BG.unsafePerformIO hs_bindgen_99da7108788a2cf4

-- __unique:__ @test_functionsfun_attributes_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_a0d2ec6ffc23fc5a" hs_bindgen_a0d2ec6ffc23fc5a_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f1@
hs_bindgen_a0d2ec6ffc23fc5a :: IO (BG.FunPtr (IO ()))
hs_bindgen_a0d2ec6ffc23fc5a =
  BG.fromFFIType hs_bindgen_a0d2ec6ffc23fc5a_base

{-# NOINLINE f1 #-}
{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h 19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: BG.FunPtr (IO ())
f1 = BG.unsafePerformIO hs_bindgen_a0d2ec6ffc23fc5a

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memalign@
foreign import ccall unsafe "hs_bindgen_16a2a5a807cc26ec" hs_bindgen_16a2a5a807cc26ec_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memalign@
hs_bindgen_16a2a5a807cc26ec :: IO (BG.FunPtr (Size_t -> Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_16a2a5a807cc26ec =
  BG.fromFFIType hs_bindgen_16a2a5a807cc26ec_base

{-# NOINLINE my_memalign #-}
{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h 23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign :: BG.FunPtr (Size_t -> Size_t -> IO (BG.Ptr BG.Void))
my_memalign =
  BG.unsafePerformIO hs_bindgen_16a2a5a807cc26ec

-- __unique:__ @test_functionsfun_attributes_Example_get_my_calloc@
foreign import ccall unsafe "hs_bindgen_976df97f9c9c1223" hs_bindgen_976df97f9c9c1223_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_calloc@
hs_bindgen_976df97f9c9c1223 :: IO (BG.FunPtr (Size_t -> Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_976df97f9c9c1223 =
  BG.fromFFIType hs_bindgen_976df97f9c9c1223_base

{-# NOINLINE my_calloc #-}
{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h 28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc :: BG.FunPtr (Size_t -> Size_t -> IO (BG.Ptr BG.Void))
my_calloc =
  BG.unsafePerformIO hs_bindgen_976df97f9c9c1223

-- __unique:__ @test_functionsfun_attributes_Example_get_my_realloc@
foreign import ccall unsafe "hs_bindgen_4021d3c6eae30c8c" hs_bindgen_4021d3c6eae30c8c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_realloc@
hs_bindgen_4021d3c6eae30c8c :: IO (BG.FunPtr (BG.Ptr BG.Void -> Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_4021d3c6eae30c8c =
  BG.fromFFIType hs_bindgen_4021d3c6eae30c8c_base

{-# NOINLINE my_realloc #-}
{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h 29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc :: BG.FunPtr (BG.Ptr BG.Void -> Size_t -> IO (BG.Ptr BG.Void))
my_realloc =
  BG.unsafePerformIO hs_bindgen_4021d3c6eae30c8c

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc1@
foreign import ccall unsafe "hs_bindgen_2ca35d0827c76ff7" hs_bindgen_2ca35d0827c76ff7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc1@
hs_bindgen_2ca35d0827c76ff7 :: IO (BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_2ca35d0827c76ff7 =
  BG.fromFFIType hs_bindgen_2ca35d0827c76ff7_base

{-# NOINLINE my_alloc1 #-}
{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h 34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 :: BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void))
my_alloc1 =
  BG.unsafePerformIO hs_bindgen_2ca35d0827c76ff7

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc2@
foreign import ccall unsafe "hs_bindgen_a43746fed9206f42" hs_bindgen_a43746fed9206f42_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_alloc2@
hs_bindgen_a43746fed9206f42 :: IO (BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_a43746fed9206f42 =
  BG.fromFFIType hs_bindgen_a43746fed9206f42_base

{-# NOINLINE my_alloc2 #-}
{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h 35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 :: BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void))
my_alloc2 =
  BG.unsafePerformIO hs_bindgen_a43746fed9206f42

-- __unique:__ @test_functionsfun_attributes_Example_get_square@
foreign import ccall unsafe "hs_bindgen_d41557222fd2ab94" hs_bindgen_d41557222fd2ab94_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_square@
hs_bindgen_d41557222fd2ab94 :: IO (BG.FunPtr (BG.CInt -> IO BG.CInt))
hs_bindgen_d41557222fd2ab94 =
  BG.fromFFIType hs_bindgen_d41557222fd2ab94_base

{-# NOINLINE square #-}
{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h 39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square :: BG.FunPtr (BG.CInt -> IO BG.CInt)
square =
  BG.unsafePerformIO hs_bindgen_d41557222fd2ab94

-- __unique:__ @test_functionsfun_attributes_Example_get_old_fn_deprecated@
foreign import ccall unsafe "hs_bindgen_beee43878922a900" hs_bindgen_beee43878922a900_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_old_fn_deprecated@
hs_bindgen_beee43878922a900 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_beee43878922a900 =
  BG.fromFFIType hs_bindgen_beee43878922a900_base

{-# NOINLINE old_fn_deprecated #-}
{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h 48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: BG.FunPtr (IO BG.CInt)
old_fn_deprecated =
  BG.unsafePerformIO hs_bindgen_beee43878922a900

-- __unique:__ @test_functionsfun_attributes_Example_get_my_dgettext@
foreign import ccall unsafe "hs_bindgen_f95f43d50ed8f2b4" hs_bindgen_f95f43d50ed8f2b4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_dgettext@
hs_bindgen_f95f43d50ed8f2b4 :: IO (BG.FunPtr (BG.Ptr BG.CChar -> PtrConst.PtrConst BG.CChar -> IO (BG.Ptr BG.CChar)))
hs_bindgen_f95f43d50ed8f2b4 =
  BG.fromFFIType hs_bindgen_f95f43d50ed8f2b4_base

{-# NOINLINE my_dgettext #-}
{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h 64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext :: BG.FunPtr (BG.Ptr BG.CChar -> PtrConst.PtrConst BG.CChar -> IO (BG.Ptr BG.CChar))
my_dgettext =
  BG.unsafePerformIO hs_bindgen_f95f43d50ed8f2b4

-- __unique:__ @test_functionsfun_attributes_Example_get_fdopen@
foreign import ccall unsafe "hs_bindgen_986260288574592d" hs_bindgen_986260288574592d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fdopen@
hs_bindgen_986260288574592d :: IO (BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CChar -> IO (BG.Ptr FILE)))
hs_bindgen_986260288574592d =
  BG.fromFFIType hs_bindgen_986260288574592d_base

{-# NOINLINE fdopen #-}
{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h 75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen :: BG.FunPtr (BG.CInt -> PtrConst.PtrConst BG.CChar -> IO (BG.Ptr FILE))
fdopen =
  BG.unsafePerformIO hs_bindgen_986260288574592d

-- __unique:__ @test_functionsfun_attributes_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_fa90d405c15977a2" hs_bindgen_fa90d405c15977a2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f2@
hs_bindgen_fa90d405c15977a2 :: IO (BG.FunPtr (IO ()))
hs_bindgen_fa90d405c15977a2 =
  BG.fromFFIType hs_bindgen_fa90d405c15977a2_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h 79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: BG.FunPtr (IO ())
f2 = BG.unsafePerformIO hs_bindgen_fa90d405c15977a2

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memcpy@
foreign import ccall unsafe "hs_bindgen_481e3a75205c85f2" hs_bindgen_481e3a75205c85f2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_my_memcpy@
hs_bindgen_481e3a75205c85f2 :: IO (BG.FunPtr (BG.Ptr BG.Void -> PtrConst.PtrConst BG.Void -> Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_481e3a75205c85f2 =
  BG.fromFFIType hs_bindgen_481e3a75205c85f2_base

{-# NOINLINE my_memcpy #-}
{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h 85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy :: BG.FunPtr (BG.Ptr BG.Void -> PtrConst.PtrConst BG.Void -> Size_t -> IO (BG.Ptr BG.Void))
my_memcpy =
  BG.unsafePerformIO hs_bindgen_481e3a75205c85f2

-- __unique:__ @test_functionsfun_attributes_Example_get_fatal@
foreign import ccall unsafe "hs_bindgen_99cc62837ebec657" hs_bindgen_99cc62837ebec657_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fatal@
hs_bindgen_99cc62837ebec657 :: IO (BG.FunPtr (IO ()))
hs_bindgen_99cc62837ebec657 =
  BG.fromFFIType hs_bindgen_99cc62837ebec657_base

{-# NOINLINE fatal #-}
{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h 102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: BG.FunPtr (IO ())
fatal =
  BG.unsafePerformIO hs_bindgen_99cc62837ebec657

-- __unique:__ @test_functionsfun_attributes_Example_get_hash@
foreign import ccall unsafe "hs_bindgen_196cbfce2a9df94f" hs_bindgen_196cbfce2a9df94f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_hash@
hs_bindgen_196cbfce2a9df94f :: IO (BG.FunPtr (BG.Ptr BG.CChar -> IO BG.CInt))
hs_bindgen_196cbfce2a9df94f =
  BG.fromFFIType hs_bindgen_196cbfce2a9df94f_base

{-# NOINLINE hash #-}
{-| __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h 110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash :: BG.FunPtr (BG.Ptr BG.CChar -> IO BG.CInt)
hash = BG.unsafePerformIO hs_bindgen_196cbfce2a9df94f

-- __unique:__ @test_functionsfun_attributes_Example_get_mymalloc@
foreign import ccall unsafe "hs_bindgen_ae6b503283507bd2" hs_bindgen_ae6b503283507bd2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_mymalloc@
hs_bindgen_ae6b503283507bd2 :: IO (BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void)))
hs_bindgen_ae6b503283507bd2 =
  BG.fromFFIType hs_bindgen_ae6b503283507bd2_base

{-# NOINLINE mymalloc #-}
{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h 115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc :: BG.FunPtr (Size_t -> IO (BG.Ptr BG.Void))
mymalloc =
  BG.unsafePerformIO hs_bindgen_ae6b503283507bd2

-- __unique:__ @test_functionsfun_attributes_Example_get_foobar@
foreign import ccall unsafe "hs_bindgen_ad3e5c88477c9e92" hs_bindgen_ad3e5c88477c9e92_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_foobar@
hs_bindgen_ad3e5c88477c9e92 :: IO (BG.FunPtr (IO ()))
hs_bindgen_ad3e5c88477c9e92 =
  BG.fromFFIType hs_bindgen_ad3e5c88477c9e92_base

{-# NOINLINE foobar #-}
{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h 119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: BG.FunPtr (IO ())
foobar =
  BG.unsafePerformIO hs_bindgen_ad3e5c88477c9e92

-- __unique:__ @test_functionsfun_attributes_Example_get_core2_func@
foreign import ccall unsafe "hs_bindgen_508f28109ff2076e" hs_bindgen_508f28109ff2076e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_core2_func@
hs_bindgen_508f28109ff2076e :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_508f28109ff2076e =
  BG.fromFFIType hs_bindgen_508f28109ff2076e_base

{-# NOINLINE core2_func #-}
{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h 126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: BG.FunPtr (IO BG.CInt)
core2_func =
  BG.unsafePerformIO hs_bindgen_508f28109ff2076e

-- __unique:__ @test_functionsfun_attributes_Example_get_sse3_func@
foreign import ccall unsafe "hs_bindgen_6e4f5b53b36a3ecb" hs_bindgen_6e4f5b53b36a3ecb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_sse3_func@
hs_bindgen_6e4f5b53b36a3ecb :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_6e4f5b53b36a3ecb =
  BG.fromFFIType hs_bindgen_6e4f5b53b36a3ecb_base

{-# NOINLINE sse3_func #-}
{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h 127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: BG.FunPtr (IO BG.CInt)
sse3_func =
  BG.unsafePerformIO hs_bindgen_6e4f5b53b36a3ecb

-- __unique:__ @test_functionsfun_attributes_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_e39d5186f5da6c3f" hs_bindgen_e39d5186f5da6c3f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_f3@
hs_bindgen_e39d5186f5da6c3f :: IO (BG.FunPtr (IO ()))
hs_bindgen_e39d5186f5da6c3f =
  BG.fromFFIType hs_bindgen_e39d5186f5da6c3f_base

{-# NOINLINE f3 #-}
{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h 131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: BG.FunPtr (IO ())
f3 = BG.unsafePerformIO hs_bindgen_e39d5186f5da6c3f

-- __unique:__ @test_functionsfun_attributes_Example_get_fn@
foreign import ccall unsafe "hs_bindgen_acb0e567f47dc0eb" hs_bindgen_acb0e567f47dc0eb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_fn@
hs_bindgen_acb0e567f47dc0eb :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_acb0e567f47dc0eb =
  BG.fromFFIType hs_bindgen_acb0e567f47dc0eb_base

{-# NOINLINE fn #-}
{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h 136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: BG.FunPtr (IO BG.CInt)
fn = BG.unsafePerformIO hs_bindgen_acb0e567f47dc0eb

-- __unique:__ @test_functionsfun_attributes_Example_get_y@
foreign import ccall unsafe "hs_bindgen_a1f58949b267b87f" hs_bindgen_a1f58949b267b87f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_y@
hs_bindgen_a1f58949b267b87f :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_a1f58949b267b87f =
  BG.fromFFIType hs_bindgen_a1f58949b267b87f_base

{-# NOINLINE y #-}
{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h 142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: BG.FunPtr (IO BG.CInt)
y = BG.unsafePerformIO hs_bindgen_a1f58949b267b87f

-- __unique:__ @test_functionsfun_attributes_Example_get_x1@
foreign import ccall unsafe "hs_bindgen_06bd05eb2df3a936" hs_bindgen_06bd05eb2df3a936_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_x1@
hs_bindgen_06bd05eb2df3a936 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_06bd05eb2df3a936 =
  BG.fromFFIType hs_bindgen_06bd05eb2df3a936_base

{-# NOINLINE x1 #-}
{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h 145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: BG.FunPtr (IO BG.CInt)
x1 = BG.unsafePerformIO hs_bindgen_06bd05eb2df3a936

-- __unique:__ @test_functionsfun_attributes_Example_get_x2@
foreign import ccall unsafe "hs_bindgen_f2ae2154ea32940b" hs_bindgen_f2ae2154ea32940b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_get_x2@
hs_bindgen_f2ae2154ea32940b :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_f2ae2154ea32940b =
  BG.fromFFIType hs_bindgen_f2ae2154ea32940b_base

{-# NOINLINE x2 #-}
{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h 148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: BG.FunPtr (IO BG.CInt)
x2 = BG.unsafePerformIO hs_bindgen_f2ae2154ea32940b
