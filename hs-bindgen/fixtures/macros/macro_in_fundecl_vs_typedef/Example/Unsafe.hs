{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_803859b9d5ea2750 ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_04808dc3142a2b13 ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_f38ce43ca615b384 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ac128ba1294115de ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_4ec091f1df8e7484 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_4724be655c57783c ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a45f9b7c237f52c2 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_12604d239d0c3498 ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ebab9061525626c9 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0de7b47f3ac3b7bd ("
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

    __unique:__ @Example_Unsafe_quux1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_803859b9d5ea2750" quux1 ::
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

    __unique:__ @Example_Unsafe_quux2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_04808dc3142a2b13" quux2 ::
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

    __unique:__ @Example_Unsafe_wam1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_f38ce43ca615b384" wam1 ::
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

    __unique:__ @Example_Unsafe_wam2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ac128ba1294115de" wam2 ::
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

    __unique:__ @Example_Unsafe_struct_typedef1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_4ec091f1df8e7484" struct_typedef1 ::
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

    __unique:__ @Example_Unsafe_struct_typedef2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_4724be655c57783c" struct_typedef2 ::
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

    __unique:__ @Example_Unsafe_struct_typedef3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_a45f9b7c237f52c2" struct_typedef3 ::
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

    __unique:__ @Example_Unsafe_struct_name1@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_12604d239d0c3498" struct_name1 ::
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

    __unique:__ @Example_Unsafe_struct_name2@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_ebab9061525626c9" struct_name2 ::
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

    __unique:__ @Example_Unsafe_struct_name3@
-}
foreign import ccall unsafe "hs_bindgen_test_macrosmacro_in_fundecl_vs_typ_0de7b47f3ac3b7bd" struct_name3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
