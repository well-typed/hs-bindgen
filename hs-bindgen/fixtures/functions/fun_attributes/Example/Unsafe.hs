{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "void hs_bindgen_52759f125bf2b140 (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_80bb9d1445e894ca (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_ebf8d1f009064640 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_a062d8e757dc6824 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_899561850b80c305 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_d5eb45f9de991bca ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_a7aa3949fa7cae3f ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_dbe49279b6585cea ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_f51c36dd7e8f4133 (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_bf6f222178bd7c31 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_830629dc11c2fdfc ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_a5f34f5beb1c74f1 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_0f3586df383dffea ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_667d3280d945cd0c (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_394fd662d5fb7aa6 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_5594a84fb65782e1 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_1f19397195b32853 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_f80f9b58791a9cf2 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_6a951361c18a91a0 (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_1d7f2cdf95b3bfa3 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_c1fff017165ba0e1 (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_67dc9f91fbda20c7 (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_8562db8b96c10d6b (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_150a79fec58eaf56 (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_52759f125bf2b140" __f1_base ::
     IO ()

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe___f1@
-}
__f1 ::
     IO ()
__f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType __f1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_80bb9d1445e894ca" f1_base ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_f1@
-}
f1 ::
     IO ()
f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ebf8d1f009064640" my_memalign_base ::
     FC.CInt
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memalign@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_memalign =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_memalign_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a062d8e757dc6824" my_calloc_base ::
     FC.CInt
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_calloc@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_calloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_calloc_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_899561850b80c305" my_realloc_base ::
     Ptr.Ptr Void
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_realloc@
-}
my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_realloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_realloc_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_d5eb45f9de991bca" my_alloc1_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc1@
-}
my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_alloc1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a7aa3949fa7cae3f" my_alloc2_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc2@
-}
my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_alloc2_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_dbe49279b6585cea" square_base ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_square@
-}
square ::
     FC.CInt
  -> FC.CInt
square =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType square_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f51c36dd7e8f4133" old_fn_deprecated_base ::
     IO FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_old_fn_deprecated@
-}
old_fn_deprecated ::
     IO FC.CInt
old_fn_deprecated =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType old_fn_deprecated_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_bf6f222178bd7c31" my_dgettext_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_dgettext@
-}
my_dgettext ::
     Ptr.Ptr FC.CChar
     -- ^ __C declaration:__ @my_domain@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
     -- ^ __C declaration:__ @my_format@
  -> IO (Ptr.Ptr FC.CChar)
my_dgettext =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_dgettext_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_830629dc11c2fdfc" fdopen_base ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_fdopen@
-}
fdopen ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO (Ptr.Ptr FILE)
fdopen =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fdopen_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a5f34f5beb1c74f1" f2_base ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_f2@
-}
f2 ::
     IO ()
f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f2_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0f3586df383dffea" my_memcpy_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memcpy@
-}
my_memcpy ::
     Ptr.Ptr Void
     -- ^ __C declaration:__ @dest@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
     -- ^ __C declaration:__ @src@
  -> Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
my_memcpy =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_memcpy_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_667d3280d945cd0c" fatal_base ::
     IO ()

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_fatal@
-}
fatal ::
     IO ()
fatal =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fatal_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_394fd662d5fb7aa6" hash_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@

__unique:__ @test_functionsfun_attributes_Example_Unsafe_hash@
-}
hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hash =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hash_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_5594a84fb65782e1" mymalloc_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_mymalloc@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
mymalloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType mymalloc_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1f19397195b32853" foobar_base ::
     IO ()

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_foobar@
-}
foobar ::
     IO ()
foobar =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType foobar_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f80f9b58791a9cf2" core2_func_base ::
     IO FC.CInt

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_core2_func@
-}
core2_func ::
     IO FC.CInt
core2_func =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType core2_func_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6a951361c18a91a0" sse3_func_base ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_sse3_func@
-}
sse3_func ::
     IO FC.CInt
sse3_func =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType sse3_func_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1d7f2cdf95b3bfa3" f3_base ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_f3@
-}
f3 ::
     IO ()
f3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f3_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c1fff017165ba0e1" fn_base ::
     IO FC.CInt

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_fn@
-}
fn ::
     IO FC.CInt
fn =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fn_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_67dc9f91fbda20c7" y_base ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_y@
-}
y ::
     IO FC.CInt
y =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType y_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8562db8b96c10d6b" x1_base ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_x1@
-}
x1 ::
     IO FC.CInt
x1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType x1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_150a79fec58eaf56" x2_base ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Unsafe_x2@
-}
x2 ::
     IO FC.CInt
x2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType x2_base
