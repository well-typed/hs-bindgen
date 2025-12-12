{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

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
  , "void hs_bindgen_0560fe42a40f777f (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_1a4676387075dc40 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_63adcb061045c5ac ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_733b29547ce864f6 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_4c69efa2a8a2b7c0 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_1eae846583dd415c ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_790482b4016de326 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_3f72dedf649beccd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_a8f71f2272dae572 (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_11a623401451cca5 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_30143e337a327ef0 ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7b2c420d0febf062 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_af1f131d9e98a2ff ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_0afa6ff8226517c8 (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_948fc14ee9d5d56f ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_60517fb6ae2517ff ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_f1451b46f1bd3813 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_07f5843dd5c65611 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_2c7c9e9a45042696 (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_4ff2d7abd6099082 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_c3ae037518ec9b4e (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_da9708096863a242 (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_037c35609f7728b3 (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_2c3e8d78049741c3 (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0560fe42a40f777f" __f1_base ::
     IO ()

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe___f1@
-}
__f1 ::
     IO ()
__f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType __f1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_1a4676387075dc40" f1_base ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_f1@
-}
f1 ::
     IO ()
f1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_63adcb061045c5ac" my_memalign_base ::
     FC.CInt
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_memalign@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_memalign =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_memalign_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_733b29547ce864f6" my_calloc_base ::
     FC.CInt
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_calloc@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_calloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_calloc_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_4c69efa2a8a2b7c0" my_realloc_base ::
     Ptr.Ptr Void
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_realloc@
-}
my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_realloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_realloc_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_1eae846583dd415c" my_alloc1_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc1@
-}
my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_alloc1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_790482b4016de326" my_alloc2_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc2@
-}
my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType my_alloc2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_3f72dedf649beccd" square_base ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_square@
-}
square ::
     FC.CInt
  -> FC.CInt
square =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType square_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_a8f71f2272dae572" old_fn_deprecated_base ::
     IO FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_old_fn_deprecated@
-}
old_fn_deprecated ::
     IO FC.CInt
old_fn_deprecated =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType old_fn_deprecated_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_11a623401451cca5" my_dgettext_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_dgettext@
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
foreign import ccall safe "hs_bindgen_30143e337a327ef0" fdopen_base ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_fdopen@
-}
fdopen ::
     FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CChar
  -> IO (Ptr.Ptr FILE)
fdopen =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fdopen_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7b2c420d0febf062" f2_base ::
     IO ()

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_f2@
-}
f2 ::
     IO ()
f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_af1f131d9e98a2ff" my_memcpy_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_my_memcpy@
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
foreign import ccall safe "hs_bindgen_0afa6ff8226517c8" fatal_base ::
     IO ()

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_fatal@
-}
fatal ::
     IO ()
fatal =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fatal_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_948fc14ee9d5d56f" hash_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@

__unique:__ @test_functionsfun_attributes_Example_Safe_hash@
-}
hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hash =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hash_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_60517fb6ae2517ff" mymalloc_base ::
     FC.CInt
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_mymalloc@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
mymalloc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType mymalloc_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_f1451b46f1bd3813" foobar_base ::
     IO ()

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_foobar@
-}
foobar ::
     IO ()
foobar =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType foobar_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_07f5843dd5c65611" core2_func_base ::
     IO FC.CInt

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_core2_func@
-}
core2_func ::
     IO FC.CInt
core2_func =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType core2_func_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2c7c9e9a45042696" sse3_func_base ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_sse3_func@
-}
sse3_func ::
     IO FC.CInt
sse3_func =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType sse3_func_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_4ff2d7abd6099082" f3_base ::
     IO ()

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_f3@
-}
f3 ::
     IO ()
f3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType f3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_c3ae037518ec9b4e" fn_base ::
     IO FC.CInt

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_fn@
-}
fn ::
     IO FC.CInt
fn =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fn_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_da9708096863a242" y_base ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_y@
-}
y ::
     IO FC.CInt
y =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType y_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_037c35609f7728b3" x1_base ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_x1@
-}
x1 ::
     IO FC.CInt
x1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType x1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2c3e8d78049741c3" x2_base ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @test_functionsfun_attributes_Example_Safe_x2@
-}
x2 ::
     IO FC.CInt
x2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType x2_base
