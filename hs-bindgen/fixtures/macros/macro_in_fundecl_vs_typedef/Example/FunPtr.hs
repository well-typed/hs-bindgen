{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_in_fundecl_vs_typedef.h>"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_quux1 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_8c77c87c8985f5b4 (void)) ("
  , "  MC arg1,"
  , "  TC arg2"
  , ")"
  , "{"
  , "  return &quux1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_quux2 */"
  , "__attribute__ ((const))"
  , "TC (*hs_bindgen_ad1c057fd2e38491 (void)) ("
  , "  MC arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_wam1 */"
  , "__attribute__ ((const))"
  , "MC *(*hs_bindgen_05b0ef45be2bc377 (void)) ("
  , "  float arg1,"
  , "  TC *arg2"
  , ")"
  , "{"
  , "  return &wam1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_wam2 */"
  , "__attribute__ ((const))"
  , "TC *(*hs_bindgen_cd643a7a3c18ccf6 (void)) ("
  , "  float arg1,"
  , "  MC *arg2"
  , ")"
  , "{"
  , "  return &wam2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0322539d81aee229 (void)) ("
  , "  struct2 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8522498cba39ada6 (void)) ("
  , "  struct3_t *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_e19fc7f7dee46424 (void)) ("
  , "  struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_typedef3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_00f81edda453ec64 (void)) ("
  , "  struct struct1 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8f4de3d502f3466d (void)) ("
  , "  struct struct3 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_ba133ae0ff062248 (void)) ("
  , "  struct struct4 *arg1,"
  , "  MC arg2"
  , ")"
  , "{"
  , "  return &struct_name3;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux1@
foreign import ccall unsafe "hs_bindgen_8c77c87c8985f5b4" hs_bindgen_8c77c87c8985f5b4_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux1@
hs_bindgen_8c77c87c8985f5b4 :: IO (RIP.FunPtr (MC -> TC -> IO RIP.CChar))
hs_bindgen_8c77c87c8985f5b4 =
  RIP.fromFFIType hs_bindgen_8c77c87c8985f5b4_base

{-# NOINLINE quux1 #-}
{-| __C declaration:__ @quux1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 8:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux1 :: RIP.FunPtr (MC -> TC -> IO RIP.CChar)
quux1 =
  RIP.unsafePerformIO hs_bindgen_8c77c87c8985f5b4

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux2@
foreign import ccall unsafe "hs_bindgen_ad1c057fd2e38491" hs_bindgen_ad1c057fd2e38491_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_quux2@
hs_bindgen_ad1c057fd2e38491 :: IO (RIP.FunPtr (MC -> RIP.CChar -> IO TC))
hs_bindgen_ad1c057fd2e38491 =
  RIP.fromFFIType hs_bindgen_ad1c057fd2e38491_base

{-# NOINLINE quux2 #-}
{-| __C declaration:__ @quux2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 9:4@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
quux2 :: RIP.FunPtr (MC -> RIP.CChar -> IO TC)
quux2 =
  RIP.unsafePerformIO hs_bindgen_ad1c057fd2e38491

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam1@
foreign import ccall unsafe "hs_bindgen_05b0ef45be2bc377" hs_bindgen_05b0ef45be2bc377_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam1@
hs_bindgen_05b0ef45be2bc377 :: IO (RIP.FunPtr (RIP.CFloat -> (RIP.Ptr TC) -> IO (RIP.Ptr MC)))
hs_bindgen_05b0ef45be2bc377 =
  RIP.fromFFIType hs_bindgen_05b0ef45be2bc377_base

{-# NOINLINE wam1 #-}
{-| __C declaration:__ @wam1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 10:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam1 :: RIP.FunPtr (RIP.CFloat -> (RIP.Ptr TC) -> IO (RIP.Ptr MC))
wam1 =
  RIP.unsafePerformIO hs_bindgen_05b0ef45be2bc377

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam2@
foreign import ccall unsafe "hs_bindgen_cd643a7a3c18ccf6" hs_bindgen_cd643a7a3c18ccf6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_wam2@
hs_bindgen_cd643a7a3c18ccf6 :: IO (RIP.FunPtr (RIP.CFloat -> (RIP.Ptr MC) -> IO (RIP.Ptr TC)))
hs_bindgen_cd643a7a3c18ccf6 =
  RIP.fromFFIType hs_bindgen_cd643a7a3c18ccf6_base

{-# NOINLINE wam2 #-}
{-| __C declaration:__ @wam2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 11:5@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
wam2 :: RIP.FunPtr (RIP.CFloat -> (RIP.Ptr MC) -> IO (RIP.Ptr TC))
wam2 =
  RIP.unsafePerformIO hs_bindgen_cd643a7a3c18ccf6

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef1@
foreign import ccall unsafe "hs_bindgen_0322539d81aee229" hs_bindgen_0322539d81aee229_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef1@
hs_bindgen_0322539d81aee229 :: IO (RIP.FunPtr ((RIP.Ptr Struct2) -> MC -> IO ()))
hs_bindgen_0322539d81aee229 =
  RIP.fromFFIType hs_bindgen_0322539d81aee229_base

{-# NOINLINE struct_typedef1 #-}
{-| __C declaration:__ @struct_typedef1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 23:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef1 :: RIP.FunPtr ((RIP.Ptr Struct2) -> MC -> IO ())
struct_typedef1 =
  RIP.unsafePerformIO hs_bindgen_0322539d81aee229

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef2@
foreign import ccall unsafe "hs_bindgen_8522498cba39ada6" hs_bindgen_8522498cba39ada6_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef2@
hs_bindgen_8522498cba39ada6 :: IO (RIP.FunPtr ((RIP.Ptr Struct3_t) -> MC -> IO ()))
hs_bindgen_8522498cba39ada6 =
  RIP.fromFFIType hs_bindgen_8522498cba39ada6_base

{-# NOINLINE struct_typedef2 #-}
{-| __C declaration:__ @struct_typedef2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 24:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef2 :: RIP.FunPtr ((RIP.Ptr Struct3_t) -> MC -> IO ())
struct_typedef2 =
  RIP.unsafePerformIO hs_bindgen_8522498cba39ada6

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef3@
foreign import ccall unsafe "hs_bindgen_e19fc7f7dee46424" hs_bindgen_e19fc7f7dee46424_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_typedef3@
hs_bindgen_e19fc7f7dee46424 :: IO (RIP.FunPtr ((RIP.Ptr Struct4) -> MC -> IO ()))
hs_bindgen_e19fc7f7dee46424 =
  RIP.fromFFIType hs_bindgen_e19fc7f7dee46424_base

{-# NOINLINE struct_typedef3 #-}
{-| __C declaration:__ @struct_typedef3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 25:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_typedef3 :: RIP.FunPtr ((RIP.Ptr Struct4) -> MC -> IO ())
struct_typedef3 =
  RIP.unsafePerformIO hs_bindgen_e19fc7f7dee46424

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name1@
foreign import ccall unsafe "hs_bindgen_00f81edda453ec64" hs_bindgen_00f81edda453ec64_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name1@
hs_bindgen_00f81edda453ec64 :: IO (RIP.FunPtr ((RIP.Ptr Struct1) -> MC -> IO ()))
hs_bindgen_00f81edda453ec64 =
  RIP.fromFFIType hs_bindgen_00f81edda453ec64_base

{-# NOINLINE struct_name1 #-}
{-| __C declaration:__ @struct_name1@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 27:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name1 :: RIP.FunPtr ((RIP.Ptr Struct1) -> MC -> IO ())
struct_name1 =
  RIP.unsafePerformIO hs_bindgen_00f81edda453ec64

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name2@
foreign import ccall unsafe "hs_bindgen_8f4de3d502f3466d" hs_bindgen_8f4de3d502f3466d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name2@
hs_bindgen_8f4de3d502f3466d :: IO (RIP.FunPtr ((RIP.Ptr Struct3) -> MC -> IO ()))
hs_bindgen_8f4de3d502f3466d =
  RIP.fromFFIType hs_bindgen_8f4de3d502f3466d_base

{-# NOINLINE struct_name2 #-}
{-| __C declaration:__ @struct_name2@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 28:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name2 :: RIP.FunPtr ((RIP.Ptr Struct3) -> MC -> IO ())
struct_name2 =
  RIP.unsafePerformIO hs_bindgen_8f4de3d502f3466d

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name3@
foreign import ccall unsafe "hs_bindgen_ba133ae0ff062248" hs_bindgen_ba133ae0ff062248_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_vs_typ_Example_get_struct_name3@
hs_bindgen_ba133ae0ff062248 :: IO (RIP.FunPtr ((RIP.Ptr Struct4) -> MC -> IO ()))
hs_bindgen_ba133ae0ff062248 =
  RIP.fromFFIType hs_bindgen_ba133ae0ff062248_base

{-# NOINLINE struct_name3 #-}
{-| __C declaration:__ @struct_name3@

    __defined at:__ @macros\/macro_in_fundecl_vs_typedef.h 29:6@

    __exported by:__ @macros\/macro_in_fundecl_vs_typedef.h@
-}
struct_name3 :: RIP.FunPtr ((RIP.Ptr Struct4) -> MC -> IO ())
struct_name3 =
  RIP.unsafePerformIO hs_bindgen_ba133ae0ff062248
