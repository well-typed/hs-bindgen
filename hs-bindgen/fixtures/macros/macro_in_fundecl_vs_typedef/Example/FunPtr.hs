{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_quux1_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_fb1d9bc73e620f06 (void)) ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return &quux1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_quux2_ptr */"
  , "__attribute__ ((const))"
  , "TC (*hs_bindgen_9dc824587cab07a2 (void)) ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_wam1_ptr */"
  , "__attribute__ ((const))"
  , "MC *(*hs_bindgen_03d794639e412075 (void)) ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return &wam1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_wam2_ptr */"
  , "__attribute__ ((const))"
  , "TC *(*hs_bindgen_0a5eb04fc739212a (void)) ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return &wam2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6d4f35a86a00c68b (void)) ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_af92d1197a77fe13 (void)) ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c5effcd02d3d5efd (void)) ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name1_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_842a0d61a7a895d6 (void)) ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name2_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ead25a696827a8f7 (void)) ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name3_ptr */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_680e8a5d673ce9c1 (void)) ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name3;"
  , "}"
  ]))

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux1_ptr@
foreign import ccall unsafe "hs_bindgen_fb1d9bc73e620f06" hs_bindgen_fb1d9bc73e620f06 ::
     IO (Ptr.FunPtr (MC -> TC -> IO FC.CChar))

{-# NOINLINE quux1_ptr #-}

{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1_ptr :: Ptr.FunPtr (MC -> TC -> IO FC.CChar)
quux1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fb1d9bc73e620f06

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux2_ptr@
foreign import ccall unsafe "hs_bindgen_9dc824587cab07a2" hs_bindgen_9dc824587cab07a2 ::
     IO (Ptr.FunPtr (MC -> FC.CChar -> IO TC))

{-# NOINLINE quux2_ptr #-}

{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2_ptr :: Ptr.FunPtr (MC -> FC.CChar -> IO TC)
quux2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9dc824587cab07a2

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam1_ptr@
foreign import ccall unsafe "hs_bindgen_03d794639e412075" hs_bindgen_03d794639e412075 ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC)))

{-# NOINLINE wam1_ptr #-}

{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr TC) -> IO (Ptr.Ptr MC))
wam1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_03d794639e412075

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam2_ptr@
foreign import ccall unsafe "hs_bindgen_0a5eb04fc739212a" hs_bindgen_0a5eb04fc739212a ::
     IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC)))

{-# NOINLINE wam2_ptr #-}

{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2_ptr :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr MC) -> IO (Ptr.Ptr TC))
wam2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0a5eb04fc739212a

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef1_ptr@
foreign import ccall unsafe "hs_bindgen_6d4f35a86a00c68b" hs_bindgen_6d4f35a86a00c68b ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ()))

{-# NOINLINE struct_typedef1_ptr #-}

{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct2) -> MC -> IO ())
struct_typedef1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6d4f35a86a00c68b

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef2_ptr@
foreign import ccall unsafe "hs_bindgen_af92d1197a77fe13" hs_bindgen_af92d1197a77fe13 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ()))

{-# NOINLINE struct_typedef2_ptr #-}

{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_af92d1197a77fe13

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef3_ptr@
foreign import ccall unsafe "hs_bindgen_c5effcd02d3d5efd" hs_bindgen_c5effcd02d3d5efd ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_typedef3_ptr #-}

{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_typedef3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c5effcd02d3d5efd

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name1_ptr@
foreign import ccall unsafe "hs_bindgen_842a0d61a7a895d6" hs_bindgen_842a0d61a7a895d6 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ()))

{-# NOINLINE struct_name1_ptr #-}

{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct1) -> MC -> IO ())
struct_name1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_842a0d61a7a895d6

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name2_ptr@
foreign import ccall unsafe "hs_bindgen_ead25a696827a8f7" hs_bindgen_ead25a696827a8f7 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ()))

{-# NOINLINE struct_name2_ptr #-}

{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct3) -> MC -> IO ())
struct_name2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ead25a696827a8f7

-- | __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name3_ptr@
foreign import ccall unsafe "hs_bindgen_680e8a5d673ce9c1" hs_bindgen_680e8a5d673ce9c1 ::
     IO (Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ()))

{-# NOINLINE struct_name3_ptr #-}

{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h:29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3_ptr :: Ptr.FunPtr ((Ptr.Ptr Struct4) -> MC -> IO ())
struct_name3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_680e8a5d673ce9c1
