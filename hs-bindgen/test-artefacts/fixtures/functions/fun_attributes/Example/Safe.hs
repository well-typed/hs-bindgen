{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.__f1
    , Example.Safe.f1
    , Example.Safe.my_memalign
    , Example.Safe.my_calloc
    , Example.Safe.my_realloc
    , Example.Safe.my_alloc1
    , Example.Safe.my_alloc2
    , Example.Safe.square
    , Example.Safe.old_fn_deprecated
    , Example.Safe.my_dgettext
    , Example.Safe.fdopen
    , Example.Safe.f2
    , Example.Safe.my_memcpy
    , Example.Safe.fatal
    , Example.Safe.hash
    , Example.Safe.mymalloc
    , Example.Safe.foobar
    , Example.Safe.core2_func
    , Example.Safe.sse3_func
    , Example.Safe.f3
    , Example.Safe.fn
    , Example.Safe.y
    , Example.Safe.x1
    , Example.Safe.x2
    )
  where

import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_attributes.h>"
  , "void hs_bindgen_0560fe42a40f777f (void)"
  , "{"
  , "  (__f1)();"
  , "}"
  , "void hs_bindgen_1a4676387075dc40 (void)"
  , "{"
  , "  (f1)();"
  , "}"
  , "void *hs_bindgen_63adcb061045c5ac ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_memalign)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_733b29547ce864f6 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_calloc)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_4c69efa2a8a2b7c0 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return (my_realloc)(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_1eae846583dd415c ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (my_alloc1)(arg1);"
  , "}"
  , "void *hs_bindgen_790482b4016de326 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (my_alloc2)(arg1);"
  , "}"
  , "signed int hs_bindgen_3f72dedf649beccd ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return (square)(arg1);"
  , "}"
  , "signed int hs_bindgen_a8f71f2272dae572 (void)"
  , "{"
  , "  return (old_fn_deprecated)();"
  , "}"
  , "char *hs_bindgen_11a623401451cca5 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (my_dgettext)(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_30143e337a327ef0 ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return (fdopen)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7b2c420d0febf062 (void)"
  , "{"
  , "  (f2)();"
  , "}"
  , "void *hs_bindgen_af1f131d9e98a2ff ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return (my_memcpy)(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_0afa6ff8226517c8 (void)"
  , "{"
  , "  (fatal)();"
  , "}"
  , "signed int hs_bindgen_948fc14ee9d5d56f ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return (hash)(arg1);"
  , "}"
  , "void *hs_bindgen_60517fb6ae2517ff ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return (mymalloc)(arg1);"
  , "}"
  , "void hs_bindgen_f1451b46f1bd3813 (void)"
  , "{"
  , "  (foobar)();"
  , "}"
  , "signed int hs_bindgen_07f5843dd5c65611 (void)"
  , "{"
  , "  return (core2_func)();"
  , "}"
  , "signed int hs_bindgen_2c7c9e9a45042696 (void)"
  , "{"
  , "  return (sse3_func)();"
  , "}"
  , "void hs_bindgen_4ff2d7abd6099082 (void)"
  , "{"
  , "  (f3)();"
  , "}"
  , "signed int hs_bindgen_c3ae037518ec9b4e (void)"
  , "{"
  , "  return (fn)();"
  , "}"
  , "signed int hs_bindgen_da9708096863a242 (void)"
  , "{"
  , "  return (y)();"
  , "}"
  , "signed int hs_bindgen_037c35609f7728b3 (void)"
  , "{"
  , "  return (x1)();"
  , "}"
  , "signed int hs_bindgen_2c3e8d78049741c3 (void)"
  , "{"
  , "  return (x2)();"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_attributes_Example_Safe___f1@
foreign import ccall safe "hs_bindgen_0560fe42a40f777f" hs_bindgen_0560fe42a40f777f_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe___f1@
hs_bindgen_0560fe42a40f777f :: IO ()
hs_bindgen_0560fe42a40f777f =
  BG.fromFFIType hs_bindgen_0560fe42a40f777f_base

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
  BG.fromFFIType hs_bindgen_1a4676387075dc40_base

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h 19:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
f1 :: IO ()
f1 = hs_bindgen_1a4676387075dc40

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memalign@
foreign import ccall safe "hs_bindgen_63adcb061045c5ac" hs_bindgen_63adcb061045c5ac_base ::
     BG.Int32
  -> BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memalign@
hs_bindgen_63adcb061045c5ac ::
     Size_t
  -> Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_63adcb061045c5ac =
  BG.fromFFIType hs_bindgen_63adcb061045c5ac_base

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h 23:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memalign ::
     Size_t
  -> Size_t
  -> IO (BG.Ptr BG.Void)
my_memalign = hs_bindgen_63adcb061045c5ac

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_calloc@
foreign import ccall safe "hs_bindgen_733b29547ce864f6" hs_bindgen_733b29547ce864f6_base ::
     BG.Int32
  -> BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_calloc@
hs_bindgen_733b29547ce864f6 ::
     Size_t
  -> Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_733b29547ce864f6 =
  BG.fromFFIType hs_bindgen_733b29547ce864f6_base

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h 28:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_calloc ::
     Size_t
  -> Size_t
  -> IO (BG.Ptr BG.Void)
my_calloc = hs_bindgen_733b29547ce864f6

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_realloc@
foreign import ccall safe "hs_bindgen_4c69efa2a8a2b7c0" hs_bindgen_4c69efa2a8a2b7c0_base ::
     BG.Ptr BG.Void
  -> BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_realloc@
hs_bindgen_4c69efa2a8a2b7c0 ::
     BG.Ptr BG.Void
  -> Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_4c69efa2a8a2b7c0 =
  BG.fromFFIType hs_bindgen_4c69efa2a8a2b7c0_base

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h 29:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_realloc ::
     BG.Ptr BG.Void
  -> Size_t
  -> IO (BG.Ptr BG.Void)
my_realloc = hs_bindgen_4c69efa2a8a2b7c0

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc1@
foreign import ccall safe "hs_bindgen_1eae846583dd415c" hs_bindgen_1eae846583dd415c_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc1@
hs_bindgen_1eae846583dd415c ::
     Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_1eae846583dd415c =
  BG.fromFFIType hs_bindgen_1eae846583dd415c_base

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h 34:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc1 ::
     Size_t
  -> IO (BG.Ptr BG.Void)
my_alloc1 = hs_bindgen_1eae846583dd415c

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc2@
foreign import ccall safe "hs_bindgen_790482b4016de326" hs_bindgen_790482b4016de326_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_alloc2@
hs_bindgen_790482b4016de326 ::
     Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_790482b4016de326 =
  BG.fromFFIType hs_bindgen_790482b4016de326_base

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h 35:7@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_alloc2 ::
     Size_t
  -> IO (BG.Ptr BG.Void)
my_alloc2 = hs_bindgen_790482b4016de326

-- __unique:__ @test_functionsfun_attributes_Example_Safe_square@
foreign import ccall safe "hs_bindgen_3f72dedf649beccd" hs_bindgen_3f72dedf649beccd_base ::
     BG.Int32
  -> BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_square@
hs_bindgen_3f72dedf649beccd ::
     BG.CInt
  -> BG.CInt
hs_bindgen_3f72dedf649beccd =
  BG.fromFFIType hs_bindgen_3f72dedf649beccd_base

{-|

    Marked @__attribute((const))__@

    __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h 39:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
square ::
     BG.CInt
  -> BG.CInt
square = hs_bindgen_3f72dedf649beccd

-- __unique:__ @test_functionsfun_attributes_Example_Safe_old_fn_deprecated@
foreign import ccall safe "hs_bindgen_a8f71f2272dae572" hs_bindgen_a8f71f2272dae572_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_old_fn_deprecated@
hs_bindgen_a8f71f2272dae572 :: IO BG.CInt
hs_bindgen_a8f71f2272dae572 =
  BG.fromFFIType hs_bindgen_a8f71f2272dae572_base

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h 48:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
old_fn_deprecated :: IO BG.CInt
old_fn_deprecated = hs_bindgen_a8f71f2272dae572

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_dgettext@
foreign import ccall safe "hs_bindgen_11a623401451cca5" hs_bindgen_11a623401451cca5_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_dgettext@
hs_bindgen_11a623401451cca5 ::
     BG.Ptr BG.CChar
  -> PtrConst.PtrConst BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_11a623401451cca5 =
  BG.fromFFIType hs_bindgen_11a623401451cca5_base

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h 64:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_dgettext ::
     BG.Ptr BG.CChar
     -- ^ __C declaration:__ @my_domain@
  -> PtrConst.PtrConst BG.CChar
     -- ^ __C declaration:__ @my_format@
  -> IO (BG.Ptr BG.CChar)
my_dgettext = hs_bindgen_11a623401451cca5

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fdopen@
foreign import ccall safe "hs_bindgen_30143e337a327ef0" hs_bindgen_30143e337a327ef0_base ::
     BG.Int32
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fdopen@
hs_bindgen_30143e337a327ef0 ::
     BG.CInt
  -> PtrConst.PtrConst BG.CChar
  -> IO (BG.Ptr FILE)
hs_bindgen_30143e337a327ef0 =
  BG.fromFFIType hs_bindgen_30143e337a327ef0_base

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h 75:9@

    __exported by:__ @functions\/fun_attributes.h@
-}
fdopen ::
     BG.CInt
  -> PtrConst.PtrConst BG.CChar
  -> IO (BG.Ptr FILE)
fdopen = hs_bindgen_30143e337a327ef0

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f2@
foreign import ccall safe "hs_bindgen_7b2c420d0febf062" hs_bindgen_7b2c420d0febf062_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f2@
hs_bindgen_7b2c420d0febf062 :: IO ()
hs_bindgen_7b2c420d0febf062 =
  BG.fromFFIType hs_bindgen_7b2c420d0febf062_base

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h 79:65@

    __exported by:__ @functions\/fun_attributes.h@
-}
f2 :: IO ()
f2 = hs_bindgen_7b2c420d0febf062

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memcpy@
foreign import ccall safe "hs_bindgen_af1f131d9e98a2ff" hs_bindgen_af1f131d9e98a2ff_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_my_memcpy@
hs_bindgen_af1f131d9e98a2ff ::
     BG.Ptr BG.Void
  -> PtrConst.PtrConst BG.Void
  -> Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_af1f131d9e98a2ff =
  BG.fromFFIType hs_bindgen_af1f131d9e98a2ff_base

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h 85:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
my_memcpy ::
     BG.Ptr BG.Void
     -- ^ __C declaration:__ @dest@
  -> PtrConst.PtrConst BG.Void
     -- ^ __C declaration:__ @src@
  -> Size_t
     -- ^ __C declaration:__ @len@
  -> IO (BG.Ptr BG.Void)
my_memcpy = hs_bindgen_af1f131d9e98a2ff

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fatal@
foreign import ccall safe "hs_bindgen_0afa6ff8226517c8" hs_bindgen_0afa6ff8226517c8_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fatal@
hs_bindgen_0afa6ff8226517c8 :: IO ()
hs_bindgen_0afa6ff8226517c8 =
  BG.fromFFIType hs_bindgen_0afa6ff8226517c8_base

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h 102:6@

    __exported by:__ @functions\/fun_attributes.h@
-}
fatal :: IO ()
fatal = hs_bindgen_0afa6ff8226517c8

-- __unique:__ @test_functionsfun_attributes_Example_Safe_hash@
foreign import ccall safe "hs_bindgen_948fc14ee9d5d56f" hs_bindgen_948fc14ee9d5d56f_base ::
     BG.Ptr BG.Void
  -> IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_hash@
hs_bindgen_948fc14ee9d5d56f ::
     BG.Ptr BG.CChar
  -> IO BG.CInt
hs_bindgen_948fc14ee9d5d56f =
  BG.fromFFIType hs_bindgen_948fc14ee9d5d56f_base

{-|

    Marked @__attribute((pure))__@

    __C declaration:__ @hash@

    __defined at:__ @functions\/fun_attributes.h 110:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
hash ::
     BG.Ptr BG.CChar
  -> IO BG.CInt
hash = hs_bindgen_948fc14ee9d5d56f

-- __unique:__ @test_functionsfun_attributes_Example_Safe_mymalloc@
foreign import ccall safe "hs_bindgen_60517fb6ae2517ff" hs_bindgen_60517fb6ae2517ff_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_functionsfun_attributes_Example_Safe_mymalloc@
hs_bindgen_60517fb6ae2517ff ::
     Size_t
  -> IO (BG.Ptr BG.Void)
hs_bindgen_60517fb6ae2517ff =
  BG.fromFFIType hs_bindgen_60517fb6ae2517ff_base

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h 115:1@

    __exported by:__ @functions\/fun_attributes.h@
-}
mymalloc ::
     Size_t
     -- ^ __C declaration:__ @len@
  -> IO (BG.Ptr BG.Void)
mymalloc = hs_bindgen_60517fb6ae2517ff

-- __unique:__ @test_functionsfun_attributes_Example_Safe_foobar@
foreign import ccall safe "hs_bindgen_f1451b46f1bd3813" hs_bindgen_f1451b46f1bd3813_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_foobar@
hs_bindgen_f1451b46f1bd3813 :: IO ()
hs_bindgen_f1451b46f1bd3813 =
  BG.fromFFIType hs_bindgen_f1451b46f1bd3813_base

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h 119:13@

    __exported by:__ @functions\/fun_attributes.h@
-}
foobar :: IO ()
foobar = hs_bindgen_f1451b46f1bd3813

-- __unique:__ @test_functionsfun_attributes_Example_Safe_core2_func@
foreign import ccall safe "hs_bindgen_07f5843dd5c65611" hs_bindgen_07f5843dd5c65611_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_core2_func@
hs_bindgen_07f5843dd5c65611 :: IO BG.CInt
hs_bindgen_07f5843dd5c65611 =
  BG.fromFFIType hs_bindgen_07f5843dd5c65611_base

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h 126:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
core2_func :: IO BG.CInt
core2_func = hs_bindgen_07f5843dd5c65611

-- __unique:__ @test_functionsfun_attributes_Example_Safe_sse3_func@
foreign import ccall safe "hs_bindgen_2c7c9e9a45042696" hs_bindgen_2c7c9e9a45042696_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_sse3_func@
hs_bindgen_2c7c9e9a45042696 :: IO BG.CInt
hs_bindgen_2c7c9e9a45042696 =
  BG.fromFFIType hs_bindgen_2c7c9e9a45042696_base

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h 127:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
sse3_func :: IO BG.CInt
sse3_func = hs_bindgen_2c7c9e9a45042696

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f3@
foreign import ccall safe "hs_bindgen_4ff2d7abd6099082" hs_bindgen_4ff2d7abd6099082_base ::
     IO ()

-- __unique:__ @test_functionsfun_attributes_Example_Safe_f3@
hs_bindgen_4ff2d7abd6099082 :: IO ()
hs_bindgen_4ff2d7abd6099082 =
  BG.fromFFIType hs_bindgen_4ff2d7abd6099082_base

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h 131:49@

    __exported by:__ @functions\/fun_attributes.h@
-}
f3 :: IO ()
f3 = hs_bindgen_4ff2d7abd6099082

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fn@
foreign import ccall safe "hs_bindgen_c3ae037518ec9b4e" hs_bindgen_c3ae037518ec9b4e_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_fn@
hs_bindgen_c3ae037518ec9b4e :: IO BG.CInt
hs_bindgen_c3ae037518ec9b4e =
  BG.fromFFIType hs_bindgen_c3ae037518ec9b4e_base

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h 136:5@

    __exported by:__ @functions\/fun_attributes.h@
-}
fn :: IO BG.CInt
fn = hs_bindgen_c3ae037518ec9b4e

-- __unique:__ @test_functionsfun_attributes_Example_Safe_y@
foreign import ccall safe "hs_bindgen_da9708096863a242" hs_bindgen_da9708096863a242_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_y@
hs_bindgen_da9708096863a242 :: IO BG.CInt
hs_bindgen_da9708096863a242 =
  BG.fromFFIType hs_bindgen_da9708096863a242_base

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h 142:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
y :: IO BG.CInt
y = hs_bindgen_da9708096863a242

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x1@
foreign import ccall safe "hs_bindgen_037c35609f7728b3" hs_bindgen_037c35609f7728b3_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x1@
hs_bindgen_037c35609f7728b3 :: IO BG.CInt
hs_bindgen_037c35609f7728b3 =
  BG.fromFFIType hs_bindgen_037c35609f7728b3_base

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h 145:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x1 :: IO BG.CInt
x1 = hs_bindgen_037c35609f7728b3

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x2@
foreign import ccall safe "hs_bindgen_2c3e8d78049741c3" hs_bindgen_2c3e8d78049741c3_base ::
     IO BG.Int32

-- __unique:__ @test_functionsfun_attributes_Example_Safe_x2@
hs_bindgen_2c3e8d78049741c3 :: IO BG.CInt
hs_bindgen_2c3e8d78049741c3 =
  BG.fromFFIType hs_bindgen_2c3e8d78049741c3_base

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h 148:12@

    __exported by:__ @functions\/fun_attributes.h@
-}
x2 :: IO BG.CInt
x2 = hs_bindgen_2c3e8d78049741c3
