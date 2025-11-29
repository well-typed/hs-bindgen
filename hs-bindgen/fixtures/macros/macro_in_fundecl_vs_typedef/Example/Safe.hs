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
  , "char hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_bfbfa81b329867fd ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d3679c8538c7c532 ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_197ed2c4feefa892 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_bda817b5febf6570 ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_c99f55bec3f4b4f2 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_123e4cd2bda243e9 ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_f420e5b883cbf082 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_13a8455401331495 ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7b24db9cbb9c211a ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_fc0611c0655be463 ("
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

    __unique:__ @Example_Safe_quux1@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_bfbfa81b329867fd" quux1 ::
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

    __unique:__ @Example_Safe_quux2@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_d3679c8538c7c532" quux2 ::
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

    __unique:__ @Example_Safe_wam1@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_197ed2c4feefa892" wam1 ::
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

    __unique:__ @Example_Safe_wam2@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_bda817b5febf6570" wam2 ::
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

    __unique:__ @Example_Safe_struct_typedef1@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_c99f55bec3f4b4f2" struct_typedef1 ::
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

    __unique:__ @Example_Safe_struct_typedef2@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_123e4cd2bda243e9" struct_typedef2 ::
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

    __unique:__ @Example_Safe_struct_typedef3@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_f420e5b883cbf082" struct_typedef3 ::
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

    __unique:__ @Example_Safe_struct_name1@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_13a8455401331495" struct_name1 ::
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

    __unique:__ @Example_Safe_struct_name2@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_7b24db9cbb9c211a" struct_name2 ::
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

    __unique:__ @Example_Safe_struct_name3@
-}
foreign import ccall safe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_fc0611c0655be463" struct_name3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
