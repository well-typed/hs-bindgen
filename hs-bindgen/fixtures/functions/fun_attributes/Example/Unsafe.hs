{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
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

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe___f1@
foreign import ccall unsafe "hs_bindgen_52759f125bf2b140" hs_bindgen_52759f125bf2b140 ::
     IO ()

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1 :: IO ()
__f1 = hs_bindgen_52759f125bf2b140

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f1@
foreign import ccall unsafe "hs_bindgen_80bb9d1445e894ca" hs_bindgen_80bb9d1445e894ca ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_80bb9d1445e894ca

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memalign@
foreign import ccall unsafe "hs_bindgen_ebf8d1f009064640" hs_bindgen_ebf8d1f009064640 ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_memalign = hs_bindgen_ebf8d1f009064640

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_calloc@
foreign import ccall unsafe "hs_bindgen_a062d8e757dc6824" hs_bindgen_a062d8e757dc6824 ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_calloc = hs_bindgen_a062d8e757dc6824

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_realloc@
foreign import ccall unsafe "hs_bindgen_899561850b80c305" hs_bindgen_899561850b80c305 ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_realloc = hs_bindgen_899561850b80c305

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc1@
foreign import ccall unsafe "hs_bindgen_d5eb45f9de991bca" hs_bindgen_d5eb45f9de991bca ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc1 = hs_bindgen_d5eb45f9de991bca

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_alloc2@
foreign import ccall unsafe "hs_bindgen_a7aa3949fa7cae3f" hs_bindgen_a7aa3949fa7cae3f ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc2 = hs_bindgen_a7aa3949fa7cae3f

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_square@
foreign import ccall unsafe "hs_bindgen_dbe49279b6585cea" hs_bindgen_dbe49279b6585cea ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square ::
     FC.CInt
  -> FC.CInt
square = hs_bindgen_dbe49279b6585cea

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_old_fn_deprecated@
foreign import ccall unsafe "hs_bindgen_f51c36dd7e8f4133" hs_bindgen_f51c36dd7e8f4133 ::
     IO FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: IO FC.CInt
old_fn_deprecated = hs_bindgen_f51c36dd7e8f4133

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_dgettext@
foreign import ccall unsafe "hs_bindgen_bf6f222178bd7c31" hs_bindgen_bf6f222178bd7c31 ::
     Ptr.Ptr FC.CChar
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext ::
     Ptr.Ptr FC.CChar
     -- ^ __C declaration:__ @my_domain@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
     -- ^ __C declaration:__ @my_format@
  -> IO (Ptr.Ptr FC.CChar)
my_dgettext = hs_bindgen_bf6f222178bd7c31

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fdopen@
foreign import ccall unsafe "hs_bindgen_830629dc11c2fdfc" hs_bindgen_830629dc11c2fdfc ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO (Ptr.Ptr FILE)
fdopen = hs_bindgen_830629dc11c2fdfc

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f2@
foreign import ccall unsafe "hs_bindgen_a5f34f5beb1c74f1" hs_bindgen_a5f34f5beb1c74f1 ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_a5f34f5beb1c74f1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_my_memcpy@
foreign import ccall unsafe "hs_bindgen_0f3586df383dffea" hs_bindgen_0f3586df383dffea ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy ::
     Ptr.Ptr Void
     -- ^ __C declaration:__ @dest@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
     -- ^ __C declaration:__ @src@
  -> Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
my_memcpy = hs_bindgen_0f3586df383dffea

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fatal@
foreign import ccall unsafe "hs_bindgen_667d3280d945cd0c" hs_bindgen_667d3280d945cd0c ::
     IO ()

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: IO ()
fatal = hs_bindgen_667d3280d945cd0c

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_hash@
foreign import ccall unsafe "hs_bindgen_394fd662d5fb7aa6" hs_bindgen_394fd662d5fb7aa6 ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@
-}
hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hash = hs_bindgen_394fd662d5fb7aa6

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_mymalloc@
foreign import ccall unsafe "hs_bindgen_5594a84fb65782e1" hs_bindgen_5594a84fb65782e1 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
mymalloc = hs_bindgen_5594a84fb65782e1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_foobar@
foreign import ccall unsafe "hs_bindgen_1f19397195b32853" hs_bindgen_1f19397195b32853 ::
     IO ()

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: IO ()
foobar = hs_bindgen_1f19397195b32853

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_core2_func@
foreign import ccall unsafe "hs_bindgen_f80f9b58791a9cf2" hs_bindgen_f80f9b58791a9cf2 ::
     IO FC.CInt

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: IO FC.CInt
core2_func = hs_bindgen_f80f9b58791a9cf2

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_sse3_func@
foreign import ccall unsafe "hs_bindgen_6a951361c18a91a0" hs_bindgen_6a951361c18a91a0 ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: IO FC.CInt
sse3_func = hs_bindgen_6a951361c18a91a0

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_f3@
foreign import ccall unsafe "hs_bindgen_1d7f2cdf95b3bfa3" hs_bindgen_1d7f2cdf95b3bfa3 ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_1d7f2cdf95b3bfa3

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_fn@
foreign import ccall unsafe "hs_bindgen_c1fff017165ba0e1" hs_bindgen_c1fff017165ba0e1 ::
     IO FC.CInt

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: IO FC.CInt
fn = hs_bindgen_c1fff017165ba0e1

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_y@
foreign import ccall unsafe "hs_bindgen_67dc9f91fbda20c7" hs_bindgen_67dc9f91fbda20c7 ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: IO FC.CInt
y = hs_bindgen_67dc9f91fbda20c7

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x1@
foreign import ccall unsafe "hs_bindgen_8562db8b96c10d6b" hs_bindgen_8562db8b96c10d6b ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: IO FC.CInt
x1 = hs_bindgen_8562db8b96c10d6b

-- __unique:__ @test_functionsfun_attributes_Example_Unsafe_x2@
foreign import ccall unsafe "hs_bindgen_150a79fec58eaf56" hs_bindgen_150a79fec58eaf56 ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: IO FC.CInt
x2 = hs_bindgen_150a79fec58eaf56
