{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_c7ba346f3006b36f ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_db114519a8645d1f ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0a613fb26d413eaa ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_279b15c6940eb4f8 ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_27a965a9bfd8c176 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_8e7c55302a7b5716 ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_fe83edb35a817050 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_162df9fabcbef0c4 ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a6b5f272333b19d4 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_9eadc48880259e4a ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name3(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_c7ba346f3006b36f" quux1 ::
     MC
     {- ^ __C declaration:__ @x@
     -}
  -> TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_db114519a8645d1f" quux2 ::
     MC
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO TC

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0a613fb26d413eaa" wam1 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr MC)

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_279b15c6940eb4f8" wam2 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr MC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr TC)

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_27a965a9bfd8c176" struct_typedef1 ::
     Ptr.Ptr Struct2
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_8e7c55302a7b5716" struct_typedef2 ::
     Ptr.Ptr Struct3_t
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_fe83edb35a817050" struct_typedef3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_162df9fabcbef0c4" struct_name1 ::
     Ptr.Ptr Struct1
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a6b5f272333b19d4" struct_name2 ::
     Ptr.Ptr Struct3
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_9eadc48880259e4a" struct_name3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
