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
  [ "#include <macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_test_macro_in_fundecl_vs_typedef_d8e3530b80cd03ff ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_34300aead966e212 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_cc4abaeb55d1e034 ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_d068247e5a3b03e6 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4107c49ec0f22d0c ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_124c6e0ae4b86063 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_test_macro_in_fundecl_vs_typedef_4241bc0cd1393d99 ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name3(arg1, arg2);"
  , "}"
  ]))

{-| __C declaration:__ @quux1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d8e3530b80cd03ff" quux1 ::
     MC
     {- ^ __C declaration:__ @x@
     -}
  -> TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO FC.CChar

{-| __C declaration:__ @quux2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_857e3b8d0292d39d" quux2 ::
     MC
     {- ^ __C declaration:__ @x@
     -}
  -> FC.CChar
     {- ^ __C declaration:__ @y@
     -}
  -> IO TC

{-| __C declaration:__ @wam1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_81d53aede4a7b424" wam1 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr TC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr MC)

{-| __C declaration:__ @wam2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_035abb3b83b92fea" wam2 ::
     FC.CFloat
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr MC
     {- ^ __C declaration:__ @y@
     -}
  -> IO (Ptr.Ptr TC)

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_34300aead966e212" struct_typedef1 ::
     Ptr.Ptr Struct2
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_cc4abaeb55d1e034" struct_typedef2 ::
     Ptr.Ptr Struct3_t
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_d068247e5a3b03e6" struct_typedef3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4107c49ec0f22d0c" struct_name1 ::
     Ptr.Ptr Struct1
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_124c6e0ae4b86063" struct_name2 ::
     Ptr.Ptr Struct3
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macro_in_fundecl_vs_typedef.h@
-}
foreign import ccall unsafe "hs_bindgen_test_macro_in_fundecl_vs_typedef_4241bc0cd1393d99" struct_name3 ::
     Ptr.Ptr Struct4
     {- ^ __C declaration:__ @s@
     -}
  -> MC
     {- ^ __C declaration:__ @x@
     -}
  -> IO ()
