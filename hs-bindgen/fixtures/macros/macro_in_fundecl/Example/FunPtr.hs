{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "/* test_macrosmacro_in_fundecl_Example_get_quux */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_930b4f8122942dae (void)) ("
  , "  F arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_wam */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_31f433dd4aceda85 (void)) ("
  , "  float arg1,"
  , "  C *arg2"
  , ")"
  , "{"
  , "  return &wam;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo1 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_8d7a85b3867041bf (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo2 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_c9dcd020e5be4af9 (void)) ("
  , "  F arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_foo3 */"
  , "__attribute__ ((const))"
  , "C *(*hs_bindgen_c2295c3f61edf4e7 (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar1 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_d9b2e6e188ecbfea (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_111db9707ae2a631 (void)) ("
  , "  L arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar3 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_00996e2c7c9e793b (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  S arg1"
  , ")"
  , "{"
  , "  return &bar3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_bar4 */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_aa46ba685e4693da (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar4;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz1 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_86b147342f608069 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_a7194966e4f375df (void)) ("
  , "  I const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_baz3 */"
  , "__attribute__ ((const))"
  , "I (*(*hs_bindgen_cd69bb91c3e6d7ea (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_Example_get_no_args_no_void */"
  , "__attribute__ ((const))"
  , "I (*hs_bindgen_d5b5e70d96cc6b8d (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_quux@
foreign import ccall unsafe "hs_bindgen_930b4f8122942dae" hs_bindgen_930b4f8122942dae_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_quux@
hs_bindgen_930b4f8122942dae :: IO (RIP.FunPtr (F -> RIP.CChar -> IO RIP.CChar))
hs_bindgen_930b4f8122942dae =
  RIP.fromFFIType hs_bindgen_930b4f8122942dae_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux :: RIP.FunPtr (F -> RIP.CChar -> IO RIP.CChar)
quux =
  RIP.unsafePerformIO hs_bindgen_930b4f8122942dae

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_wam@
foreign import ccall unsafe "hs_bindgen_31f433dd4aceda85" hs_bindgen_31f433dd4aceda85_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_wam@
hs_bindgen_31f433dd4aceda85 :: IO (RIP.FunPtr (RIP.CFloat -> (RIP.Ptr C) -> IO (RIP.Ptr C)))
hs_bindgen_31f433dd4aceda85 =
  RIP.fromFFIType hs_bindgen_31f433dd4aceda85_base

{-# NOINLINE wam #-}
{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam :: RIP.FunPtr (RIP.CFloat -> (RIP.Ptr C) -> IO (RIP.Ptr C))
wam = RIP.unsafePerformIO hs_bindgen_31f433dd4aceda85

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo1@
foreign import ccall unsafe "hs_bindgen_8d7a85b3867041bf" hs_bindgen_8d7a85b3867041bf_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo1@
hs_bindgen_8d7a85b3867041bf :: IO (RIP.FunPtr (RIP.CFloat -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_8d7a85b3867041bf =
  RIP.fromFFIType hs_bindgen_8d7a85b3867041bf_base

{-# NOINLINE foo1 #-}
{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 :: RIP.FunPtr (RIP.CFloat -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr RIP.CChar))
foo1 =
  RIP.unsafePerformIO hs_bindgen_8d7a85b3867041bf

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo2@
foreign import ccall unsafe "hs_bindgen_c9dcd020e5be4af9" hs_bindgen_c9dcd020e5be4af9_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo2@
hs_bindgen_c9dcd020e5be4af9 :: IO (RIP.FunPtr (F -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr RIP.CChar)))
hs_bindgen_c9dcd020e5be4af9 =
  RIP.fromFFIType hs_bindgen_c9dcd020e5be4af9_base

{-# NOINLINE foo2 #-}
{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 :: RIP.FunPtr (F -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr RIP.CChar))
foo2 =
  RIP.unsafePerformIO hs_bindgen_c9dcd020e5be4af9

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo3@
foreign import ccall unsafe "hs_bindgen_c2295c3f61edf4e7" hs_bindgen_c2295c3f61edf4e7_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo3@
hs_bindgen_c2295c3f61edf4e7 :: IO (RIP.FunPtr (RIP.CFloat -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr C)))
hs_bindgen_c2295c3f61edf4e7 =
  RIP.fromFFIType hs_bindgen_c2295c3f61edf4e7_base

{-# NOINLINE foo3 #-}
{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 :: RIP.FunPtr (RIP.CFloat -> (RIP.FunPtr (RIP.CInt -> IO RIP.CInt)) -> IO (RIP.Ptr C))
foo3 =
  RIP.unsafePerformIO hs_bindgen_c2295c3f61edf4e7

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar1@
foreign import ccall unsafe "hs_bindgen_d9b2e6e188ecbfea" hs_bindgen_d9b2e6e188ecbfea_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar1@
hs_bindgen_d9b2e6e188ecbfea :: IO (RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (RIP.CShort -> IO RIP.CInt))))
hs_bindgen_d9b2e6e188ecbfea =
  RIP.fromFFIType hs_bindgen_d9b2e6e188ecbfea_base

{-# NOINLINE bar1 #-}
{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 :: RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (RIP.CShort -> IO RIP.CInt)))
bar1 =
  RIP.unsafePerformIO hs_bindgen_d9b2e6e188ecbfea

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar2@
foreign import ccall unsafe "hs_bindgen_111db9707ae2a631" hs_bindgen_111db9707ae2a631_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar2@
hs_bindgen_111db9707ae2a631 :: IO (RIP.FunPtr (L -> IO (RIP.FunPtr (RIP.CShort -> IO RIP.CInt))))
hs_bindgen_111db9707ae2a631 =
  RIP.fromFFIType hs_bindgen_111db9707ae2a631_base

{-# NOINLINE bar2 #-}
{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 :: RIP.FunPtr (L -> IO (RIP.FunPtr (RIP.CShort -> IO RIP.CInt)))
bar2 =
  RIP.unsafePerformIO hs_bindgen_111db9707ae2a631

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar3@
foreign import ccall unsafe "hs_bindgen_00996e2c7c9e793b" hs_bindgen_00996e2c7c9e793b_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar3@
hs_bindgen_00996e2c7c9e793b :: IO (RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (S -> IO RIP.CInt))))
hs_bindgen_00996e2c7c9e793b =
  RIP.fromFFIType hs_bindgen_00996e2c7c9e793b_base

{-# NOINLINE bar3 #-}
{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 :: RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (S -> IO RIP.CInt)))
bar3 =
  RIP.unsafePerformIO hs_bindgen_00996e2c7c9e793b

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar4@
foreign import ccall unsafe "hs_bindgen_aa46ba685e4693da" hs_bindgen_aa46ba685e4693da_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar4@
hs_bindgen_aa46ba685e4693da :: IO (RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (RIP.CShort -> IO I))))
hs_bindgen_aa46ba685e4693da =
  RIP.fromFFIType hs_bindgen_aa46ba685e4693da_base

{-# NOINLINE bar4 #-}
{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 :: RIP.FunPtr (RIP.CLong -> IO (RIP.FunPtr (RIP.CShort -> IO I)))
bar4 =
  RIP.unsafePerformIO hs_bindgen_aa46ba685e4693da

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz1@
foreign import ccall unsafe "hs_bindgen_86b147342f608069" hs_bindgen_86b147342f608069_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz1@
hs_bindgen_86b147342f608069 :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) RIP.CInt)))))
hs_bindgen_86b147342f608069 =
  RIP.fromFFIType hs_bindgen_86b147342f608069_base

{-# NOINLINE baz1 #-}
{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) RIP.CInt))))
baz1 =
  RIP.unsafePerformIO hs_bindgen_86b147342f608069

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz2@
foreign import ccall unsafe "hs_bindgen_a7194966e4f375df" hs_bindgen_a7194966e4f375df_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz2@
hs_bindgen_a7194966e4f375df :: IO (RIP.FunPtr (I -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) RIP.CInt)))))
hs_bindgen_a7194966e4f375df =
  RIP.fromFFIType hs_bindgen_a7194966e4f375df_base

{-# NOINLINE baz2 #-}
{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 :: RIP.FunPtr (I -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) RIP.CInt))))
baz2 =
  RIP.unsafePerformIO hs_bindgen_a7194966e4f375df

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz3@
foreign import ccall unsafe "hs_bindgen_cd69bb91c3e6d7ea" hs_bindgen_cd69bb91c3e6d7ea_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz3@
hs_bindgen_cd69bb91c3e6d7ea :: IO (RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) I)))))
hs_bindgen_cd69bb91c3e6d7ea =
  RIP.fromFFIType hs_bindgen_cd69bb91c3e6d7ea_base

{-# NOINLINE baz3 #-}
{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 :: RIP.FunPtr (RIP.CInt -> IO (RIP.Ptr ((CA.ConstantArray 2) ((CA.ConstantArray 3) I))))
baz3 =
  RIP.unsafePerformIO hs_bindgen_cd69bb91c3e6d7ea

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_d5b5e70d96cc6b8d" hs_bindgen_d5b5e70d96cc6b8d_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_no_args_no_void@
hs_bindgen_d5b5e70d96cc6b8d :: IO (RIP.FunPtr (IO I))
hs_bindgen_d5b5e70d96cc6b8d =
  RIP.fromFFIType hs_bindgen_d5b5e70d96cc6b8d_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: RIP.FunPtr (IO I)
no_args_no_void =
  RIP.unsafePerformIO hs_bindgen_d5b5e70d96cc6b8d
