{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_02e0e3b28d470fd4 ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return quux1(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_bb79188c8775e2e4 ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return quux2(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_a398fb73645271c5 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return wam1(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_ad904da072e0711e ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return wam2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_88e976dc10571000 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_edb3806d45d7605b ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7c7f3ab0dd790fe8 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_typedef3(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2cfbb4f5834d4bcb ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name1(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c8b765fa70f95167 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name2(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0ff3632971f092bb ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  struct_name3(arg1, arg2);"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_02e0e3b28d470fd4" quux1_base ::
     FC.CChar
  -> FC.CChar
  -> IO FC.CChar

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux1@
-}
quux1 ::
     MC
     -- ^ __C declaration:__ @x@
  -> TC
     -- ^ __C declaration:__ @y@
  -> IO FC.CChar
quux1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType quux1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_bb79188c8775e2e4" quux2_base ::
     FC.CChar
  -> FC.CChar
  -> IO FC.CChar

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux2@
-}
quux2 ::
     MC
     -- ^ __C declaration:__ @x@
  -> FC.CChar
     -- ^ __C declaration:__ @y@
  -> IO TC
quux2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType quux2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_a398fb73645271c5" wam1_base ::
     FC.CFloat
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam1@
-}
wam1 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr TC
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr MC)
wam1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType wam1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_ad904da072e0711e" wam2_base ::
     FC.CFloat
  -> Ptr.Ptr Void
  -> IO (Ptr.Ptr Void)

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam2@
-}
wam2 ::
     FC.CFloat
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr MC
     -- ^ __C declaration:__ @y@
  -> IO (Ptr.Ptr TC)
wam2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType wam2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_88e976dc10571000" struct_typedef1_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef1@
-}
struct_typedef1 ::
     Ptr.Ptr Struct2
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_typedef1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_edb3806d45d7605b" struct_typedef2_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef2@
-}
struct_typedef2 ::
     Ptr.Ptr Struct3_t
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_typedef2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_7c7f3ab0dd790fe8" struct_typedef3_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef3@
-}
struct_typedef3 ::
     Ptr.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_typedef3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_2cfbb4f5834d4bcb" struct_name1_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name1@
-}
struct_name1 ::
     Ptr.Ptr Struct1
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_name1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_c8b765fa70f95167" struct_name2_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name2@
-}
struct_name2 ::
     Ptr.Ptr Struct3
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_name2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_0ff3632971f092bb" struct_name3_base ::
     Ptr.Ptr Void
  -> FC.CChar
  -> IO ()

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@

    __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name3@
-}
struct_name3 ::
     Ptr.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType struct_name3_base
