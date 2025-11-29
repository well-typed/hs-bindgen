{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "void hs_bindgen_test_functionsfun_attributes_4caeb29a9526f8b7 (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_b2cefd7907644d46 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_880ccc900a048e0e ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_1a7937ebc7e26e34 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_affa9726bdc32fd7 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_fb6136452e954d08 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_548d51b2dd7c7f1c ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_6c3adf60c1994d31 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_57c5c590d0ac143d (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_test_functionsfun_attributes_1c69daabe5a1a874 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_test_functionsfun_attributes_a7b94fe0ea32b146 ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_a58b72650fa977ee (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_0dc6f5c92bf136b8 ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_958e3078a89d08ad (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_dd64385769ff97ac ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_aafcc924fb40c907 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_90242cc2c9a48a42 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_c97a068c565f8847 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_063a44facf5f5adb (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_c1345b7446cf5382 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_2ddf675d8a0f0547 (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_c3ce9b957254b5f2 (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_152bb750f4f4f8ae (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_a7597ed4e8f91ca2 (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe___f1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_4caeb29a9526f8b7" __f1 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_f1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_b2cefd7907644d46" f1 ::
     IO ()

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_memalign@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_880ccc900a048e0e" my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_calloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_1a7937ebc7e26e34" my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_realloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_affa9726bdc32fd7" my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_alloc1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_fb6136452e954d08" my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_alloc2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_548d51b2dd7c7f1c" my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_square@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_6c3adf60c1994d31" square ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_old_fn_deprecated@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_57c5c590d0ac143d" old_fn_deprecated ::
     IO FC.CInt

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_dgettext@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_1c69daabe5a1a874" my_dgettext ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_domain@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_format@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @fdopen@

    __defined at:__ @functions\/fun_attributes.h:75:9@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_fdopen@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_a7b94fe0ea32b146" fdopen ::
     FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_f2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_a58b72650fa977ee" f2 ::
     IO ()

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_my_memcpy@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_0dc6f5c92bf136b8" my_memcpy ::
     Ptr.Ptr Void
     {- ^ __C declaration:__ @dest@
     -}
  -> Ptr.Ptr Void
     {- ^ __C declaration:__ @src@
     -}
  -> Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @fatal@

    __defined at:__ @functions\/fun_attributes.h:102:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_fatal@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_958e3078a89d08ad" fatal ::
     IO ()

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@

__unique:__ @Example_Safe_hash@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_dd64385769ff97ac" hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_mymalloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_aafcc924fb40c907" mymalloc ::
     Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_foobar@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_90242cc2c9a48a42" foobar ::
     IO ()

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_core2_func@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_c97a068c565f8847" core2_func ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_sse3_func@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_063a44facf5f5adb" sse3_func ::
     IO FC.CInt

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_f3@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_c1345b7446cf5382" f3 ::
     IO ()

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_fn@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_2ddf675d8a0f0547" fn ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_y@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_c3ce9b957254b5f2" y ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_x1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_152bb750f4f4f8ae" x1 ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Safe_x2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_a7597ed4e8f91ca2" x2 ::
     IO FC.CInt
