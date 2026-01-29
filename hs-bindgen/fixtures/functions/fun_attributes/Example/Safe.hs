{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
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

-- __unique:__ @test_functionsfun_attributes_Example_Safe___f1@
foreign import ccall safe "hs_bindgen_0560fe42a40f777f" hs_bindgen_0560fe42a40f777f_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe___f1@
hs_bindgen_0560fe42a40f777f :: IO ()
hs_bindgen_0560fe42a40f777f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0560fe42a40f777f_base

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h 16:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
__f1 :: IO ()
__f1 = hs_bindgen_0560fe42a40f777f

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f1@
foreign import ccall safe "hs_bindgen_1a4676387075dc40" hs_bindgen_1a4676387075dc40_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f1@
hs_bindgen_1a4676387075dc40 :: IO ()
hs_bindgen_1a4676387075dc40 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1a4676387075dc40_base

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h 19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_1a4676387075dc40

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memalign@
foreign import ccall safe "hs_bindgen_63adcb061045c5ac" hs_bindgen_63adcb061045c5ac_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memalign@
hs_bindgen_63adcb061045c5ac ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_63adcb061045c5ac =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_63adcb061045c5ac_base

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h 23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_memalign = hs_bindgen_63adcb061045c5ac

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_calloc@
foreign import ccall safe "hs_bindgen_733b29547ce864f6" hs_bindgen_733b29547ce864f6_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_calloc@
hs_bindgen_733b29547ce864f6 ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_733b29547ce864f6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_733b29547ce864f6_base

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h 28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_calloc = hs_bindgen_733b29547ce864f6

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_realloc@
foreign import ccall safe "hs_bindgen_4c69efa2a8a2b7c0" hs_bindgen_4c69efa2a8a2b7c0_base ::
     Ptr.Ptr Void
  -> GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_realloc@
hs_bindgen_4c69efa2a8a2b7c0 ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_4c69efa2a8a2b7c0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4c69efa2a8a2b7c0_base

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h 29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
my_realloc = hs_bindgen_4c69efa2a8a2b7c0

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc1@
foreign import ccall safe "hs_bindgen_1eae846583dd415c" hs_bindgen_1eae846583dd415c_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc1@
hs_bindgen_1eae846583dd415c ::
     Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_1eae846583dd415c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1eae846583dd415c_base

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h 34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc1 = hs_bindgen_1eae846583dd415c

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc2@
foreign import ccall safe "hs_bindgen_790482b4016de326" hs_bindgen_790482b4016de326_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc2@
hs_bindgen_790482b4016de326 ::
     Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_790482b4016de326 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_790482b4016de326_base

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h 35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)
my_alloc2 = hs_bindgen_790482b4016de326

-- __unique:__ @test_functionsfun_attributes_Example_Safe_square@
foreign import ccall safe "hs_bindgen_3f72dedf649beccd" hs_bindgen_3f72dedf649beccd_base ::
     GHC.Int.Int32
  -> GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_square@
hs_bindgen_3f72dedf649beccd ::
     FC.CInt
  -> FC.CInt
hs_bindgen_3f72dedf649beccd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3f72dedf649beccd_base

{-|

  Marked @__attribute((const))__@

__C declaration:__ @square@

__defined at:__ @functions\/fun_attributes.h 39:5@

__exported by:__ @functions\/fun_attributes.h@
-}
square ::
     FC.CInt
  -> FC.CInt
square = hs_bindgen_3f72dedf649beccd

-- __unique:__ @test_functionsfun_attributes_Example_Safe_old_fn_deprecated@
foreign import ccall safe "hs_bindgen_a8f71f2272dae572" hs_bindgen_a8f71f2272dae572_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_old_fn_deprecated@
hs_bindgen_a8f71f2272dae572 :: IO FC.CInt
hs_bindgen_a8f71f2272dae572 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a8f71f2272dae572_base

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h 48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: IO FC.CInt
old_fn_deprecated = hs_bindgen_a8f71f2272dae572

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_dgettext@
foreign import ccall safe "hs_bindgen_11a623401451cca5" hs_bindgen_11a623401451cca5_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_dgettext@
hs_bindgen_11a623401451cca5 ::
     Ptr.Ptr FC.CChar
  -> HsBindgen.Runtime.PtrConst.PtrConst FC.CChar
  -> IO (Ptr.Ptr FC.CChar)
hs_bindgen_11a623401451cca5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_11a623401451cca5_base

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h 64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext ::
     Ptr.Ptr FC.CChar
     -- ^ __C declaration:__ @my_domain@
  -> HsBindgen.Runtime.PtrConst.PtrConst FC.CChar
     -- ^ __C declaration:__ @my_format@
  -> IO (Ptr.Ptr FC.CChar)
my_dgettext = hs_bindgen_11a623401451cca5

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fdopen@
foreign import ccall safe "hs_bindgen_30143e337a327ef0" hs_bindgen_30143e337a327ef0_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fdopen@
hs_bindgen_30143e337a327ef0 ::
     FC.CInt
  -> HsBindgen.Runtime.PtrConst.PtrConst FC.CChar
  -> IO (Ptr.Ptr FILE)
hs_bindgen_30143e337a327ef0 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_30143e337a327ef0_base

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h 75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen ::
     FC.CInt
  -> HsBindgen.Runtime.PtrConst.PtrConst FC.CChar
  -> IO (Ptr.Ptr FILE)
fdopen = hs_bindgen_30143e337a327ef0

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_7b2c420d0febf062" hs_bindgen_7b2c420d0febf062_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f2@
hs_bindgen_7b2c420d0febf062 :: IO ()
hs_bindgen_7b2c420d0febf062 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7b2c420d0febf062_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h 79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_7b2c420d0febf062

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memcpy@
foreign import ccall safe "hs_bindgen_af1f131d9e98a2ff" hs_bindgen_af1f131d9e98a2ff_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memcpy@
hs_bindgen_af1f131d9e98a2ff ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.PtrConst.PtrConst Void
  -> Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_af1f131d9e98a2ff =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_af1f131d9e98a2ff_base

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h 85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy ::
     Ptr.Ptr Void
     -- ^ __C declaration:__ @dest@
  -> HsBindgen.Runtime.PtrConst.PtrConst Void
     -- ^ __C declaration:__ @src@
  -> Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
my_memcpy = hs_bindgen_af1f131d9e98a2ff

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fatal@
foreign import ccall safe "hs_bindgen_0afa6ff8226517c8" hs_bindgen_0afa6ff8226517c8_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fatal@
hs_bindgen_0afa6ff8226517c8 :: IO ()
hs_bindgen_0afa6ff8226517c8 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0afa6ff8226517c8_base

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h 102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: IO ()
fatal = hs_bindgen_0afa6ff8226517c8

-- __unique:__ @test_functionsfun_attributes_Example_Safe_hash@
foreign import ccall safe "hs_bindgen_948fc14ee9d5d56f" hs_bindgen_948fc14ee9d5d56f_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_hash@
hs_bindgen_948fc14ee9d5d56f ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hs_bindgen_948fc14ee9d5d56f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_948fc14ee9d5d56f_base

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h 110:5@

__exported by:__ @functions\/fun_attributes.h@
-}
hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt
hash = hs_bindgen_948fc14ee9d5d56f

-- __unique:__ @test_functionsfun_attributes_Example_Safe_mymalloc@
foreign import ccall safe "hs_bindgen_60517fb6ae2517ff" hs_bindgen_60517fb6ae2517ff_base ::
     GHC.Int.Int32
  -> IO (Ptr.Ptr Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_mymalloc@
hs_bindgen_60517fb6ae2517ff ::
     Size_t
  -> IO (Ptr.Ptr Void)
hs_bindgen_60517fb6ae2517ff =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_60517fb6ae2517ff_base

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h 115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (Ptr.Ptr Void)
mymalloc = hs_bindgen_60517fb6ae2517ff

-- __unique:__ @test_functionsfun_attributes_Example_Safe_foobar@
foreign import ccall safe "hs_bindgen_f1451b46f1bd3813" hs_bindgen_f1451b46f1bd3813_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_foobar@
hs_bindgen_f1451b46f1bd3813 :: IO ()
hs_bindgen_f1451b46f1bd3813 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f1451b46f1bd3813_base

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h 119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: IO ()
foobar = hs_bindgen_f1451b46f1bd3813

-- __unique:__ @test_functionsfun_attributes_Example_Safe_core2_func@
foreign import ccall safe "hs_bindgen_07f5843dd5c65611" hs_bindgen_07f5843dd5c65611_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_core2_func@
hs_bindgen_07f5843dd5c65611 :: IO FC.CInt
hs_bindgen_07f5843dd5c65611 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_07f5843dd5c65611_base

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h 126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: IO FC.CInt
core2_func = hs_bindgen_07f5843dd5c65611

-- __unique:__ @test_functionsfun_attributes_Example_Safe_sse3_func@
foreign import ccall safe "hs_bindgen_2c7c9e9a45042696" hs_bindgen_2c7c9e9a45042696_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_sse3_func@
hs_bindgen_2c7c9e9a45042696 :: IO FC.CInt
hs_bindgen_2c7c9e9a45042696 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2c7c9e9a45042696_base

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h 127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: IO FC.CInt
sse3_func = hs_bindgen_2c7c9e9a45042696

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_4ff2d7abd6099082" hs_bindgen_4ff2d7abd6099082_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f3@
hs_bindgen_4ff2d7abd6099082 :: IO ()
hs_bindgen_4ff2d7abd6099082 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4ff2d7abd6099082_base

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h 131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_4ff2d7abd6099082

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fn@
foreign import ccall safe "hs_bindgen_c3ae037518ec9b4e" hs_bindgen_c3ae037518ec9b4e_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fn@
hs_bindgen_c3ae037518ec9b4e :: IO FC.CInt
hs_bindgen_c3ae037518ec9b4e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c3ae037518ec9b4e_base

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h 136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: IO FC.CInt
fn = hs_bindgen_c3ae037518ec9b4e

-- __unique:__ @test_functionsfun_attributes_Example_Safe_y@
foreign import ccall safe "hs_bindgen_da9708096863a242" hs_bindgen_da9708096863a242_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_y@
hs_bindgen_da9708096863a242 :: IO FC.CInt
hs_bindgen_da9708096863a242 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_da9708096863a242_base

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h 142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: IO FC.CInt
y = hs_bindgen_da9708096863a242

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x1@
foreign import ccall safe "hs_bindgen_037c35609f7728b3" hs_bindgen_037c35609f7728b3_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x1@
hs_bindgen_037c35609f7728b3 :: IO FC.CInt
hs_bindgen_037c35609f7728b3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_037c35609f7728b3_base

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h 145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: IO FC.CInt
x1 = hs_bindgen_037c35609f7728b3

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x2@
foreign import ccall safe "hs_bindgen_2c3e8d78049741c3" hs_bindgen_2c3e8d78049741c3_base ::
     IO GHC.Int.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x2@
hs_bindgen_2c3e8d78049741c3 :: IO FC.CInt
hs_bindgen_2c3e8d78049741c3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2c3e8d78049741c3_base

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h 148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: IO FC.CInt
x2 = hs_bindgen_2c3e8d78049741c3
