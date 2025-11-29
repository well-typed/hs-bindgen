{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <functions/fun_attributes.h>"
  , "void hs_bindgen_test_functionsfun_attributes_cd3c38d1c777b2a5 (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_cc22dfb87e5838fe (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_29149c17976cf16b ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_1e9224e872000fe2 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_3e0b52ae851551ef ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_88b5afca4fd85d08 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_9376a471c986e1ee ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_8f23db39ce783444 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_1c098eebeb1bb5e9 (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_test_functionsfun_attributes_c00dd19745b0d4db ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_test_functionsfun_attributes_b6e12fa4d3cf8d8a ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_661cac524e84b1f7 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_45a6fe77075a30d5 ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_16ec47ef3ae211b4 (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_d37a507d0ae23019 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_b095eada25048120 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_679a9024ee44d7a5 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_5b0f554e2a9a0739 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_294791b024b40626 (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_46d6d2725c8f934c (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_c78805d1b96146a6 (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_3f8e22ae5c5df890 (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_427bac9735d17bee (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_0f7e97c372a059ab (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe___f1@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_cd3c38d1c777b2a5" __f1 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_f1@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_cc22dfb87e5838fe" f1 ::
     IO ()

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_memalign@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_29149c17976cf16b" my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_calloc@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_1e9224e872000fe2" my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_realloc@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_3e0b52ae851551ef" my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_alloc1@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_88b5afca4fd85d08" my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_alloc2@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_9376a471c986e1ee" my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_square@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_8f23db39ce783444" square ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_old_fn_deprecated@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_1c098eebeb1bb5e9" old_fn_deprecated ::
     IO FC.CInt

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_dgettext@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_c00dd19745b0d4db" my_dgettext ::
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

    __unique:__ @Example_Unsafe_fdopen@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_b6e12fa4d3cf8d8a" fdopen ::
     FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_f2@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_661cac524e84b1f7" f2 ::
     IO ()

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_my_memcpy@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_45a6fe77075a30d5" my_memcpy ::
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

    __unique:__ @Example_Unsafe_fatal@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_16ec47ef3ae211b4" fatal ::
     IO ()

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@

__unique:__ @Example_Unsafe_hash@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_d37a507d0ae23019" hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_mymalloc@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_b095eada25048120" mymalloc ::
     Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_foobar@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_679a9024ee44d7a5" foobar ::
     IO ()

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_core2_func@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_5b0f554e2a9a0739" core2_func ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_sse3_func@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_294791b024b40626" sse3_func ::
     IO FC.CInt

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_f3@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_46d6d2725c8f934c" f3 ::
     IO ()

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_fn@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_c78805d1b96146a6" fn ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_y@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_3f8e22ae5c5df890" y ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_x1@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_427bac9735d17bee" x1 ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @Example_Unsafe_x2@
-}
foreign import ccall unsafe "hs_bindgen_test_functionsfun_attributes_0f7e97c372a059ab" x2 ::
     IO FC.CInt
