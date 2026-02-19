{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <functions/fun_attributes.h>"
  , "void hs_bindgen_52759f125bf2b140 (void)"
  , "{"
  , "  (__f1)();"
  , "}"
  , "void hs_bindgen_80bb9d1445e894ca (void)"
  , "{"
  , "  (f1)();"
  , "}"
  , "void *hs_bindgen_ebf8d1f009064640 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_memalign)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_a062d8e757dc6824 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_calloc)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_899561850b80c305 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_realloc)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_d5eb45f9de991bca ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (my_alloc1)(arg1);"
  , "}"
  , "void *hs_bindgen_a7aa3949fa7cae3f ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (my_alloc2)(arg1);"
  , "}"
  , "signed int hs_bindgen_dbe49279b6585cea ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square)(arg1);"
  , "}"
  , "signed int hs_bindgen_f51c36dd7e8f4133 (void)"
  , "{"
  , "  return (old_fn_deprecated)();"
  , "}"
  , "char *hs_bindgen_bf6f222178bd7c31 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (my_dgettext)(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_830629dc11c2fdfc ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (fdopen)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5f34f5beb1c74f1 (void)"
  , "{"
  , "  (f2)();"
  , "}"
  , "void *hs_bindgen_0f3586df383dffea ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (my_memcpy)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_667d3280d945cd0c (void)"
  , "{"
  , "  (fatal)();"
  , "}"
  , "signed int hs_bindgen_394fd662d5fb7aa6 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (hash)(arg1);"
  , "}"
  , "void *hs_bindgen_5594a84fb65782e1 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (mymalloc)(arg1);"
  , "}"
  , "void hs_bindgen_1f19397195b32853 (void)"
  , "{"
  , "  (foobar)();"
  , "}"
  , "signed int hs_bindgen_f80f9b58791a9cf2 (void)"
  , "{"
  , "  return (core2_func)();"
  , "}"
  , "signed int hs_bindgen_6a951361c18a91a0 (void)"
  , "{"
  , "  return (sse3_func)();"
  , "}"
  , "void hs_bindgen_1d7f2cdf95b3bfa3 (void)"
  , "{"
  , "  (f3)();"
  , "}"
  , "signed int hs_bindgen_c1fff017165ba0e1 (void)"
  , "{"
  , "  return (fn)();"
  , "}"
  , "signed int hs_bindgen_67dc9f91fbda20c7 (void)"
  , "{"
  , "  return (y)();"
  , "}"
  , "signed int hs_bindgen_8562db8b96c10d6b (void)"
  , "{"
  , "  return (x1)();"
  , "}"
  , "signed int hs_bindgen_150a79fec58eaf56 (void)"
  , "{"
  , "  return (x2)();"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe___f1@
foreign import ccall unsafe "hs_bindgen_52759f125bf2b140" hs_bindgen_52759f125bf2b140_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe___f1@
hs_bindgen_52759f125bf2b140 :: IO ()
hs_bindgen_52759f125bf2b140 =
  RIP.fromFFIType hs_bindgen_52759f125bf2b140_base

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h 16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1 :: IO ()
__f1 = hs_bindgen_52759f125bf2b140

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_80bb9d1445e894ca" hs_bindgen_80bb9d1445e894ca_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f1@
hs_bindgen_80bb9d1445e894ca :: IO ()
hs_bindgen_80bb9d1445e894ca =
  RIP.fromFFIType hs_bindgen_80bb9d1445e894ca_base

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h 19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_80bb9d1445e894ca

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memalign@
foreign import ccall unsafe "hs_bindgen_ebf8d1f009064640" hs_bindgen_ebf8d1f009064640_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memalign@
hs_bindgen_ebf8d1f009064640 ::
     Size_t
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_ebf8d1f009064640 =
  RIP.fromFFIType hs_bindgen_ebf8d1f009064640_base

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h 23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
my_memalign = hs_bindgen_ebf8d1f009064640

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_calloc@
foreign import ccall unsafe "hs_bindgen_a062d8e757dc6824" hs_bindgen_a062d8e757dc6824_base ::
     RIP.Int32
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_calloc@
hs_bindgen_a062d8e757dc6824 ::
     Size_t
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_a062d8e757dc6824 =
  RIP.fromFFIType hs_bindgen_a062d8e757dc6824_base

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h 28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
my_calloc = hs_bindgen_a062d8e757dc6824

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_realloc@
foreign import ccall unsafe "hs_bindgen_899561850b80c305" hs_bindgen_899561850b80c305_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_realloc@
hs_bindgen_899561850b80c305 ::
     RIP.Ptr RIP.Void
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_899561850b80c305 =
  RIP.fromFFIType hs_bindgen_899561850b80c305_base

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h 29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc ::
     RIP.Ptr RIP.Void
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
my_realloc = hs_bindgen_899561850b80c305

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc1@
foreign import ccall unsafe "hs_bindgen_d5eb45f9de991bca" hs_bindgen_d5eb45f9de991bca_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc1@
hs_bindgen_d5eb45f9de991bca ::
     Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_d5eb45f9de991bca =
  RIP.fromFFIType hs_bindgen_d5eb45f9de991bca_base

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h 34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 ::
     Size_t
  -> IO (RIP.Ptr RIP.Void)
my_alloc1 = hs_bindgen_d5eb45f9de991bca

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc2@
foreign import ccall unsafe "hs_bindgen_a7aa3949fa7cae3f" hs_bindgen_a7aa3949fa7cae3f_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc2@
hs_bindgen_a7aa3949fa7cae3f ::
     Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_a7aa3949fa7cae3f =
  RIP.fromFFIType hs_bindgen_a7aa3949fa7cae3f_base

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h 35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 ::
     Size_t
  -> IO (RIP.Ptr RIP.Void)
my_alloc2 = hs_bindgen_a7aa3949fa7cae3f

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_square@
foreign import ccall unsafe "hs_bindgen_dbe49279b6585cea" hs_bindgen_dbe49279b6585cea_base ::
     RIP.Int32
  -> RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_square@
hs_bindgen_dbe49279b6585cea ::
     RIP.CInt
  -> RIP.CInt
hs_bindgen_dbe49279b6585cea =
  RIP.fromFFIType hs_bindgen_dbe49279b6585cea_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @square@

__defined at:__ @functions\/fun_attributes.h 39:5@

__exported by:__ @functions\/fun_attributes.h@
-}
square ::
     RIP.CInt
  -> RIP.CInt
square = hs_bindgen_dbe49279b6585cea

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_old_fn_deprecated@
foreign import ccall unsafe "hs_bindgen_f51c36dd7e8f4133" hs_bindgen_f51c36dd7e8f4133_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_old_fn_deprecated@
hs_bindgen_f51c36dd7e8f4133 :: IO RIP.CInt
hs_bindgen_f51c36dd7e8f4133 =
  RIP.fromFFIType hs_bindgen_f51c36dd7e8f4133_base

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h 48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: IO RIP.CInt
old_fn_deprecated = hs_bindgen_f51c36dd7e8f4133

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_dgettext@
foreign import ccall unsafe "hs_bindgen_bf6f222178bd7c31" hs_bindgen_bf6f222178bd7c31_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_dgettext@
hs_bindgen_bf6f222178bd7c31 ::
     RIP.Ptr RIP.CChar
  -> PtrConst.PtrConst RIP.CChar
  -> IO (RIP.Ptr RIP.CChar)
hs_bindgen_bf6f222178bd7c31 =
  RIP.fromFFIType hs_bindgen_bf6f222178bd7c31_base

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h 64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext ::
     RIP.Ptr RIP.CChar
     -- ^ __C declaration:__ @my_domain@
  -> PtrConst.PtrConst RIP.CChar
     -- ^ __C declaration:__ @my_format@
  -> IO (RIP.Ptr RIP.CChar)
my_dgettext = hs_bindgen_bf6f222178bd7c31

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fdopen@
foreign import ccall unsafe "hs_bindgen_830629dc11c2fdfc" hs_bindgen_830629dc11c2fdfc_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fdopen@
hs_bindgen_830629dc11c2fdfc ::
     RIP.CInt
  -> PtrConst.PtrConst RIP.CChar
  -> IO (RIP.Ptr FILE)
hs_bindgen_830629dc11c2fdfc =
  RIP.fromFFIType hs_bindgen_830629dc11c2fdfc_base

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h 75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen ::
     RIP.CInt
  -> PtrConst.PtrConst RIP.CChar
  -> IO (RIP.Ptr FILE)
fdopen = hs_bindgen_830629dc11c2fdfc

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_a5f34f5beb1c74f1" hs_bindgen_a5f34f5beb1c74f1_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f2@
hs_bindgen_a5f34f5beb1c74f1 :: IO ()
hs_bindgen_a5f34f5beb1c74f1 =
  RIP.fromFFIType hs_bindgen_a5f34f5beb1c74f1_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h 79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_a5f34f5beb1c74f1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memcpy@
foreign import ccall unsafe "hs_bindgen_0f3586df383dffea" hs_bindgen_0f3586df383dffea_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memcpy@
hs_bindgen_0f3586df383dffea ::
     RIP.Ptr RIP.Void
  -> PtrConst.PtrConst RIP.Void
  -> Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_0f3586df383dffea =
  RIP.fromFFIType hs_bindgen_0f3586df383dffea_base

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h 85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy ::
     RIP.Ptr RIP.Void
     -- ^ __C declaration:__ @dest@
  -> PtrConst.PtrConst RIP.Void
     -- ^ __C declaration:__ @src@
  -> Size_t
     -- ^ __C declaration:__ @len@
  -> IO (RIP.Ptr RIP.Void)
my_memcpy = hs_bindgen_0f3586df383dffea

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fatal@
foreign import ccall unsafe "hs_bindgen_667d3280d945cd0c" hs_bindgen_667d3280d945cd0c_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fatal@
hs_bindgen_667d3280d945cd0c :: IO ()
hs_bindgen_667d3280d945cd0c =
  RIP.fromFFIType hs_bindgen_667d3280d945cd0c_base

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h 102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: IO ()
fatal = hs_bindgen_667d3280d945cd0c

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_hash@
foreign import ccall unsafe "hs_bindgen_394fd662d5fb7aa6" hs_bindgen_394fd662d5fb7aa6_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_hash@
hs_bindgen_394fd662d5fb7aa6 ::
     RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hs_bindgen_394fd662d5fb7aa6 =
  RIP.fromFFIType hs_bindgen_394fd662d5fb7aa6_base

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h 110:5@

__exported by:__ @functions\/fun_attributes.h@
-}
hash ::
     RIP.Ptr RIP.CChar
  -> IO RIP.CInt
hash = hs_bindgen_394fd662d5fb7aa6

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_mymalloc@
foreign import ccall unsafe "hs_bindgen_5594a84fb65782e1" hs_bindgen_5594a84fb65782e1_base ::
     RIP.Int32
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_mymalloc@
hs_bindgen_5594a84fb65782e1 ::
     Size_t
  -> IO (RIP.Ptr RIP.Void)
hs_bindgen_5594a84fb65782e1 =
  RIP.fromFFIType hs_bindgen_5594a84fb65782e1_base

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h 115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (RIP.Ptr RIP.Void)
mymalloc = hs_bindgen_5594a84fb65782e1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_foobar@
foreign import ccall unsafe "hs_bindgen_1f19397195b32853" hs_bindgen_1f19397195b32853_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_foobar@
hs_bindgen_1f19397195b32853 :: IO ()
hs_bindgen_1f19397195b32853 =
  RIP.fromFFIType hs_bindgen_1f19397195b32853_base

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h 119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: IO ()
foobar = hs_bindgen_1f19397195b32853

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_core2_func@
foreign import ccall unsafe "hs_bindgen_f80f9b58791a9cf2" hs_bindgen_f80f9b58791a9cf2_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_core2_func@
hs_bindgen_f80f9b58791a9cf2 :: IO RIP.CInt
hs_bindgen_f80f9b58791a9cf2 =
  RIP.fromFFIType hs_bindgen_f80f9b58791a9cf2_base

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h 126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: IO RIP.CInt
core2_func = hs_bindgen_f80f9b58791a9cf2

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_sse3_func@
foreign import ccall unsafe "hs_bindgen_6a951361c18a91a0" hs_bindgen_6a951361c18a91a0_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_sse3_func@
hs_bindgen_6a951361c18a91a0 :: IO RIP.CInt
hs_bindgen_6a951361c18a91a0 =
  RIP.fromFFIType hs_bindgen_6a951361c18a91a0_base

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h 127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: IO RIP.CInt
sse3_func = hs_bindgen_6a951361c18a91a0

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_1d7f2cdf95b3bfa3" hs_bindgen_1d7f2cdf95b3bfa3_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f3@
hs_bindgen_1d7f2cdf95b3bfa3 :: IO ()
hs_bindgen_1d7f2cdf95b3bfa3 =
  RIP.fromFFIType hs_bindgen_1d7f2cdf95b3bfa3_base

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h 131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_1d7f2cdf95b3bfa3

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fn@
foreign import ccall unsafe "hs_bindgen_c1fff017165ba0e1" hs_bindgen_c1fff017165ba0e1_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fn@
hs_bindgen_c1fff017165ba0e1 :: IO RIP.CInt
hs_bindgen_c1fff017165ba0e1 =
  RIP.fromFFIType hs_bindgen_c1fff017165ba0e1_base

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h 136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: IO RIP.CInt
fn = hs_bindgen_c1fff017165ba0e1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_y@
foreign import ccall unsafe "hs_bindgen_67dc9f91fbda20c7" hs_bindgen_67dc9f91fbda20c7_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_y@
hs_bindgen_67dc9f91fbda20c7 :: IO RIP.CInt
hs_bindgen_67dc9f91fbda20c7 =
  RIP.fromFFIType hs_bindgen_67dc9f91fbda20c7_base

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h 142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: IO RIP.CInt
y = hs_bindgen_67dc9f91fbda20c7

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x1@
foreign import ccall unsafe "hs_bindgen_8562db8b96c10d6b" hs_bindgen_8562db8b96c10d6b_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x1@
hs_bindgen_8562db8b96c10d6b :: IO RIP.CInt
hs_bindgen_8562db8b96c10d6b =
  RIP.fromFFIType hs_bindgen_8562db8b96c10d6b_base

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h 145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: IO RIP.CInt
x1 = hs_bindgen_8562db8b96c10d6b

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x2@
foreign import ccall unsafe "hs_bindgen_150a79fec58eaf56" hs_bindgen_150a79fec58eaf56_base ::
     IO RIP.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x2@
hs_bindgen_150a79fec58eaf56 :: IO RIP.CInt
hs_bindgen_150a79fec58eaf56 =
  RIP.fromFFIType hs_bindgen_150a79fec58eaf56_base

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h 148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: IO RIP.CInt
x2 = hs_bindgen_150a79fec58eaf56
