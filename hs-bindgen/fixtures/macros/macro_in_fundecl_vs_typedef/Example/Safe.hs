{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "char hs_bindgen_02e0e3b28d470fd4 ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return (quux1)(arg1, arg2);"
  , "}"
  , "TC hs_bindgen_bb79188c8775e2e4 ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux2)(arg1, arg2);"
  , "}"
  , "MC *hs_bindgen_a398fb73645271c5 ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return (wam1)(arg1, arg2);"
  , "}"
  , "TC *hs_bindgen_ad904da072e0711e ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return (wam2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_88e976dc10571000 ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_edb3806d45d7605b ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_7c7f3ab0dd790fe8 ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_typedef3)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_2cfbb4f5834d4bcb ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name1)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_c8b765fa70f95167 ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name2)(arg1, arg2);"
  , "}"
  , "void hs_bindgen_0ff3632971f092bb ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  (struct_name3)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux1@
foreign import ccall safe "hs_bindgen_02e0e3b28d470fd4" hs_bindgen_02e0e3b28d470fd4_base ::
     RIP.Int8
  -> RIP.Int8
  -> IO RIP.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux1@
hs_bindgen_02e0e3b28d470fd4 ::
     MC
  -> TC
  -> IO RIP.CChar
hs_bindgen_02e0e3b28d470fd4 =
  RIP.fromFFIType hs_bindgen_02e0e3b28d470fd4_base

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1 ::
     MC
     -- ^ __C declaration:__ @x@
  -> TC
     -- ^ __C declaration:__ @y@
  -> IO RIP.CChar
quux1 = hs_bindgen_02e0e3b28d470fd4

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux2@
foreign import ccall safe "hs_bindgen_bb79188c8775e2e4" hs_bindgen_bb79188c8775e2e4_base ::
     RIP.Int8
  -> RIP.Int8
  -> IO RIP.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_quux2@
hs_bindgen_bb79188c8775e2e4 ::
     MC
  -> RIP.CChar
  -> IO TC
hs_bindgen_bb79188c8775e2e4 =
  RIP.fromFFIType hs_bindgen_bb79188c8775e2e4_base

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2 ::
     MC
     -- ^ __C declaration:__ @x@
  -> RIP.CChar
     -- ^ __C declaration:__ @y@
  -> IO TC
quux2 = hs_bindgen_bb79188c8775e2e4

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam1@
foreign import ccall safe "hs_bindgen_a398fb73645271c5" hs_bindgen_a398fb73645271c5_base ::
     Float
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam1@
hs_bindgen_a398fb73645271c5 ::
     RIP.CFloat
  -> RIP.Ptr TC
  -> IO (RIP.Ptr MC)
hs_bindgen_a398fb73645271c5 =
  RIP.fromFFIType hs_bindgen_a398fb73645271c5_base

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr TC
     -- ^ __C declaration:__ @y@
  -> IO (RIP.Ptr MC)
wam1 = hs_bindgen_a398fb73645271c5

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam2@
foreign import ccall safe "hs_bindgen_ad904da072e0711e" hs_bindgen_ad904da072e0711e_base ::
     Float
  -> RIP.Ptr RIP.Void
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_wam2@
hs_bindgen_ad904da072e0711e ::
     RIP.CFloat
  -> RIP.Ptr MC
  -> IO (RIP.Ptr TC)
hs_bindgen_ad904da072e0711e =
  RIP.fromFFIType hs_bindgen_ad904da072e0711e_base

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2 ::
     RIP.CFloat
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr MC
     -- ^ __C declaration:__ @y@
  -> IO (RIP.Ptr TC)
wam2 = hs_bindgen_ad904da072e0711e

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef1@
foreign import ccall safe "hs_bindgen_88e976dc10571000" hs_bindgen_88e976dc10571000_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef1@
hs_bindgen_88e976dc10571000 ::
     RIP.Ptr Struct2
  -> MC
  -> IO ()
hs_bindgen_88e976dc10571000 =
  RIP.fromFFIType hs_bindgen_88e976dc10571000_base

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1 ::
     RIP.Ptr Struct2
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef1 = hs_bindgen_88e976dc10571000

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef2@
foreign import ccall safe "hs_bindgen_edb3806d45d7605b" hs_bindgen_edb3806d45d7605b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef2@
hs_bindgen_edb3806d45d7605b ::
     RIP.Ptr Struct3_t
  -> MC
  -> IO ()
hs_bindgen_edb3806d45d7605b =
  RIP.fromFFIType hs_bindgen_edb3806d45d7605b_base

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2 ::
     RIP.Ptr Struct3_t
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef2 = hs_bindgen_edb3806d45d7605b

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef3@
foreign import ccall safe "hs_bindgen_7c7f3ab0dd790fe8" hs_bindgen_7c7f3ab0dd790fe8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_typedef3@
hs_bindgen_7c7f3ab0dd790fe8 ::
     RIP.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_7c7f3ab0dd790fe8 =
  RIP.fromFFIType hs_bindgen_7c7f3ab0dd790fe8_base

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3 ::
     RIP.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_typedef3 = hs_bindgen_7c7f3ab0dd790fe8

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name1@
foreign import ccall safe "hs_bindgen_2cfbb4f5834d4bcb" hs_bindgen_2cfbb4f5834d4bcb_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name1@
hs_bindgen_2cfbb4f5834d4bcb ::
     RIP.Ptr Struct1
  -> MC
  -> IO ()
hs_bindgen_2cfbb4f5834d4bcb =
  RIP.fromFFIType hs_bindgen_2cfbb4f5834d4bcb_base

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1 ::
     RIP.Ptr Struct1
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name1 = hs_bindgen_2cfbb4f5834d4bcb

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name2@
foreign import ccall safe "hs_bindgen_c8b765fa70f95167" hs_bindgen_c8b765fa70f95167_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name2@
hs_bindgen_c8b765fa70f95167 ::
     RIP.Ptr Struct3
  -> MC
  -> IO ()
hs_bindgen_c8b765fa70f95167 =
  RIP.fromFFIType hs_bindgen_c8b765fa70f95167_base

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2 ::
     RIP.Ptr Struct3
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name2 = hs_bindgen_c8b765fa70f95167

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name3@
foreign import ccall safe "hs_bindgen_0ff3632971f092bb" hs_bindgen_0ff3632971f092bb_base ::
     RIP.Ptr RIP.Void
  -> RIP.Int8
  -> IO ()

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_Safe_struct_name3@
hs_bindgen_0ff3632971f092bb ::
     RIP.Ptr Struct4
  -> MC
  -> IO ()
hs_bindgen_0ff3632971f092bb =
  RIP.fromFFIType hs_bindgen_0ff3632971f092bb_base

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3 ::
     RIP.Ptr Struct4
     -- ^ __C declaration:__ @s@
  -> MC
     -- ^ __C declaration:__ @x@
  -> IO ()
struct_name3 = hs_bindgen_0ff3632971f092bb
