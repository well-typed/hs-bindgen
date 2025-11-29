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
  , "void hs_bindgen_test_functionsfun_attributes_8de545512324157b (void)"
  , "{"
  , "  __f1();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_a2f84d2570ef3892 (void)"
  , "{"
  , "  f1();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_cefda6b95395d829 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_memalign(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_e25f06c3ebec2536 ("
  , "  size_t arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_calloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_51fa664668350a00 ("
  , "  void *arg1,"
  , "  size_t arg2"
  , ")"
  , "{"
  , "  return my_realloc(arg1, arg2);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_93a5d6b7d4e02c33 ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc1(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_c948fd867be322fa ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return my_alloc2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_55e5eb89e54abf83 ("
  , "  signed int arg1"
  , ")"
  , "{"
  , "  return square(arg1);"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_e9647b9c99c68776 (void)"
  , "{"
  , "  return old_fn_deprecated();"
  , "}"
  , "char *hs_bindgen_test_functionsfun_attributes_023f7813e909f518 ("
  , "  char *arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return my_dgettext(arg1, arg2);"
  , "}"
  , "FILE *hs_bindgen_test_functionsfun_attributes_e39bbd59f1c96c14 ("
  , "  signed int arg1,"
  , "  char const *arg2"
  , ")"
  , "{"
  , "  return fdopen(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_1d043de05a457e90 (void)"
  , "{"
  , "  f2();"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_4b3bfd2d72a2db5d ("
  , "  void *arg1,"
  , "  void const *arg2,"
  , "  size_t arg3"
  , ")"
  , "{"
  , "  return my_memcpy(arg1, arg2, arg3);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_348fe595d62421cf (void)"
  , "{"
  , "  fatal();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_e30754e2591f701a ("
  , "  char *arg1"
  , ")"
  , "{"
  , "  return hash(arg1);"
  , "}"
  , "void *hs_bindgen_test_functionsfun_attributes_f6f68a022a15937a ("
  , "  size_t arg1"
  , ")"
  , "{"
  , "  return mymalloc(arg1);"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_d1bf41da7ab64db1 (void)"
  , "{"
  , "  foobar();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_00405e83bcb9b271 (void)"
  , "{"
  , "  return core2_func();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_06e7d2f8bcf43684 (void)"
  , "{"
  , "  return sse3_func();"
  , "}"
  , "void hs_bindgen_test_functionsfun_attributes_e23eff1955ebb459 (void)"
  , "{"
  , "  f3();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_ef0eea5f61ef9228 (void)"
  , "{"
  , "  return fn();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_b007466f7ff1cf28 (void)"
  , "{"
  , "  return y();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_8c9825e1b20a7ea1 (void)"
  , "{"
  , "  return x1();"
  , "}"
  , "signed int hs_bindgen_test_functionsfun_attributes_c80d61b7727dab77 (void)"
  , "{"
  , "  return x2();"
  , "}"
  ]))

{-| __C declaration:__ @__f1@

    __defined at:__ @functions\/fun_attributes.h:16:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safe__f1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_8de545512324157b" __f1 ::
     IO ()

{-| __C declaration:__ @f1@

    __defined at:__ @functions\/fun_attributes.h:19:6@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safef1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_a2f84d2570ef3892" f1 ::
     IO ()

{-| __C declaration:__ @my_memalign@

    __defined at:__ @functions\/fun_attributes.h:23:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_memalign@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_cefda6b95395d829" my_memalign ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_calloc@

    __defined at:__ @functions\/fun_attributes.h:28:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_calloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_e25f06c3ebec2536" my_calloc ::
     Size_t
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_realloc@

    __defined at:__ @functions\/fun_attributes.h:29:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_realloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_51fa664668350a00" my_realloc ::
     Ptr.Ptr Void
  -> Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc1@

    __defined at:__ @functions\/fun_attributes.h:34:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_alloc1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_93a5d6b7d4e02c33" my_alloc1 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @my_alloc2@

    __defined at:__ @functions\/fun_attributes.h:35:7@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_alloc2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_c948fd867be322fa" my_alloc2 ::
     Size_t
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @square@

    __defined at:__ @functions\/fun_attributes.h:39:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safesquare@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_55e5eb89e54abf83" square ::
     FC.CInt
  -> FC.CInt

{-| __C declaration:__ @old_fn_deprecated@

    __defined at:__ @functions\/fun_attributes.h:48:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safeold_fn_deprecated@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_e9647b9c99c68776" old_fn_deprecated ::
     IO FC.CInt

{-| __C declaration:__ @my_dgettext@

    __defined at:__ @functions\/fun_attributes.h:64:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_dgettext@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_023f7813e909f518" my_dgettext ::
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

    __unique:__ @ExampleJust Safefdopen@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_e39bbd59f1c96c14" fdopen ::
     FC.CInt
  -> Ptr.Ptr FC.CChar
  -> IO (Ptr.Ptr FILE)

{-| __C declaration:__ @f2@

    __defined at:__ @functions\/fun_attributes.h:79:65@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safef2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_1d043de05a457e90" f2 ::
     IO ()

{-| __C declaration:__ @my_memcpy@

    __defined at:__ @functions\/fun_attributes.h:85:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemy_memcpy@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_4b3bfd2d72a2db5d" my_memcpy ::
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

    __unique:__ @ExampleJust Safefatal@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_348fe595d62421cf" fatal ::
     IO ()

{-|

  Marked @__attribute((pure))__@

__C declaration:__ @hash@

__defined at:__ @functions\/fun_attributes.h:110:5@

__exported by:__ @functions\/fun_attributes.h@

__unique:__ @ExampleJust Safehash@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_e30754e2591f701a" hash ::
     Ptr.Ptr FC.CChar
  -> IO FC.CInt

{-| __C declaration:__ @mymalloc@

    __defined at:__ @functions\/fun_attributes.h:115:1@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safemymalloc@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_f6f68a022a15937a" mymalloc ::
     Size_t
     {- ^ __C declaration:__ @len@
     -}
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @foobar@

    __defined at:__ @functions\/fun_attributes.h:119:13@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safefoobar@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_d1bf41da7ab64db1" foobar ::
     IO ()

{-| __C declaration:__ @core2_func@

    __defined at:__ @functions\/fun_attributes.h:126:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safecore2_func@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_00405e83bcb9b271" core2_func ::
     IO FC.CInt

{-| __C declaration:__ @sse3_func@

    __defined at:__ @functions\/fun_attributes.h:127:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safesse3_func@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_06e7d2f8bcf43684" sse3_func ::
     IO FC.CInt

{-| __C declaration:__ @f3@

    __defined at:__ @functions\/fun_attributes.h:131:49@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safef3@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_e23eff1955ebb459" f3 ::
     IO ()

{-| __C declaration:__ @fn@

    __defined at:__ @functions\/fun_attributes.h:136:5@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safefn@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_ef0eea5f61ef9228" fn ::
     IO FC.CInt

{-| __C declaration:__ @y@

    __defined at:__ @functions\/fun_attributes.h:142:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safey@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_b007466f7ff1cf28" y ::
     IO FC.CInt

{-| __C declaration:__ @x1@

    __defined at:__ @functions\/fun_attributes.h:145:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safex1@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_8c9825e1b20a7ea1" x1 ::
     IO FC.CInt

{-| __C declaration:__ @x2@

    __defined at:__ @functions\/fun_attributes.h:148:12@

    __exported by:__ @functions\/fun_attributes.h@

    __unique:__ @ExampleJust Safex2@
-}
foreign import ccall safe "hs_bindgen_test_functionsfun_attributes_c80d61b7727dab77" x2 ::
     IO FC.CInt
