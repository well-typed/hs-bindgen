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
  [ "#include <fun_attributes.h>"
  , "void hs_bindgen_test_fun_attributes_b44da51a357ae983 (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_test_fun_attributes_c1788128a5b1c813 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_9ca07f6722bd48dc ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_72df124450cc6d26 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_effc1fd567613950 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_b3544e53af074ef1 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_0b659f90fec40284 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_cb3c687f16289bb3 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_c48f18f4f06068eb (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_test_fun_attributes_d492bd76e82890da ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_test_fun_attributes_3c91a267bd66cc10 ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_fun_attributes_14361e995fb5684a (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_e8c4a96cefd6117e ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_fun_attributes_64aa41e835dbb892 (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_88887d4b5f42f079 ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_test_fun_attributes_31e6e14ecb251fa2 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_test_fun_attributes_bb77a71513994934 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_640ec5b51b0819d1 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_a1f7636643d63586 (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_test_fun_attributes_2bef032cbe15ffd0 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_8f406104a21ff66e (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_4beb0cbf65b462bd (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_ac7386c785058f4d (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_test_fun_attributes_b6f428ed915f03cc (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| __C declaration:__ @__f1@

    __defined at:__ @fun_attributes.h:16:13@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_b44da51a357ae983" __f1 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @fun_attributes.h:19:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_c1788128a5b1c813" f1 ::
     IO ()

{-| __C declaration:__ @my_memalign@

    __defined at:__ @fun_attributes.h:23:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_9ca07f6722bd48dc" my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @fun_attributes.h:28:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_72df124450cc6d26" my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @fun_attributes.h:29:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_effc1fd567613950" my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @fun_attributes.h:34:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_b3544e53af074ef1" my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @fun_attributes.h:35:7@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_0b659f90fec40284" my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @square@

    __defined at:__ @fun_attributes.h:39:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_cb3c687f16289bb3" square ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @fun_attributes.h:48:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_c48f18f4f06068eb" old_fn_deprecated ::
     IO FC.CInt

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @fun_attributes.h:64:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_d492bd76e82890da" my_dgettext ::
     Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_domain@
     -}
  -> Ptr.Ptr FC.CChar
     {- ^ __C declaration:__ @my_format@
     -}
  -> IO (Ptr.Ptr FC.CChar)

{-| __C declaration:__ @fdopen@

    __defined at:__ @fun_attributes.h:75:9@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_3c91a267bd66cc10" fdopen ::
     FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @f2@

    __defined at:__ @fun_attributes.h:79:65@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_14361e995fb5684a" f2 ::
     IO ()

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @fun_attributes.h:85:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_e8c4a96cefd6117e" my_memcpy ::
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

    __defined at:__ @fun_attributes.h:102:6@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_64aa41e835dbb892" fatal ::
     IO ()

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @fun_attributes.h:110:5@

__exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_88887d4b5f42f079" hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @mymalloc@

    __defined at:__ @fun_attributes.h:115:1@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_31e6e14ecb251fa2" mymalloc ::
     Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foobar@

    __defined at:__ @fun_attributes.h:119:13@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_bb77a71513994934" foobar ::
     IO ()

{-| __C declaration:__ @core2_func@

    __defined at:__ @fun_attributes.h:126:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_640ec5b51b0819d1" core2_func ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @fun_attributes.h:127:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_a1f7636643d63586" sse3_func ::
     IO FC.CInt

{-| __C declaration:__ @f3@

    __defined at:__ @fun_attributes.h:131:49@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_2bef032cbe15ffd0" f3 ::
     IO ()

{-| __C declaration:__ @fn@

    __defined at:__ @fun_attributes.h:136:5@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_8f406104a21ff66e" fn ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @fun_attributes.h:142:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_4beb0cbf65b462bd" y ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @fun_attributes.h:145:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_ac7386c785058f4d" x1 ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @fun_attributes.h:148:12@

    __exported by:__ @fun_attributes.h@
-}
foreign import ccall unsafe "hs_bindgen_test_fun_attributes_b6f428ed915f03cc" x2 ::
     IO FC.CInt
