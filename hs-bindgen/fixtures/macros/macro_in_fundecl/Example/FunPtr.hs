{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_quux@
hs_bindgen_930b4f8122942dae :: IO (Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar))
hs_bindgen_930b4f8122942dae =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_930b4f8122942dae_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux :: Ptr.FunPtr (F -> FC.CChar -> IO FC.CChar)
quux =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_930b4f8122942dae

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_wam@
foreign import ccall unsafe "hs_bindgen_31f433dd4aceda85" hs_bindgen_31f433dd4aceda85_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_wam@
hs_bindgen_31f433dd4aceda85 :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C)))
hs_bindgen_31f433dd4aceda85 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_31f433dd4aceda85_base

{-# NOINLINE wam #-}
{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam :: Ptr.FunPtr (FC.CFloat -> (Ptr.Ptr C) -> IO (Ptr.Ptr C))
wam =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_31f433dd4aceda85

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo1@
foreign import ccall unsafe "hs_bindgen_8d7a85b3867041bf" hs_bindgen_8d7a85b3867041bf_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo1@
hs_bindgen_8d7a85b3867041bf :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))
hs_bindgen_8d7a85b3867041bf =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_8d7a85b3867041bf_base

{-# NOINLINE foo1 #-}
{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8d7a85b3867041bf

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo2@
foreign import ccall unsafe "hs_bindgen_c9dcd020e5be4af9" hs_bindgen_c9dcd020e5be4af9_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo2@
hs_bindgen_c9dcd020e5be4af9 :: IO (Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar)))
hs_bindgen_c9dcd020e5be4af9 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c9dcd020e5be4af9_base

{-# NOINLINE foo2 #-}
{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 :: Ptr.FunPtr (F -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr FC.CChar))
foo2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9dcd020e5be4af9

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo3@
foreign import ccall unsafe "hs_bindgen_c2295c3f61edf4e7" hs_bindgen_c2295c3f61edf4e7_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_foo3@
hs_bindgen_c2295c3f61edf4e7 :: IO (Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C)))
hs_bindgen_c2295c3f61edf4e7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c2295c3f61edf4e7_base

{-# NOINLINE foo3 #-}
{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 :: Ptr.FunPtr (FC.CFloat -> (Ptr.FunPtr (FC.CInt -> IO FC.CInt)) -> IO (Ptr.Ptr C))
foo3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c2295c3f61edf4e7

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar1@
foreign import ccall unsafe "hs_bindgen_d9b2e6e188ecbfea" hs_bindgen_d9b2e6e188ecbfea_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar1@
hs_bindgen_d9b2e6e188ecbfea :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))
hs_bindgen_d9b2e6e188ecbfea =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d9b2e6e188ecbfea_base

{-# NOINLINE bar1 #-}
{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d9b2e6e188ecbfea

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar2@
foreign import ccall unsafe "hs_bindgen_111db9707ae2a631" hs_bindgen_111db9707ae2a631_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar2@
hs_bindgen_111db9707ae2a631 :: IO (Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt))))
hs_bindgen_111db9707ae2a631 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_111db9707ae2a631_base

{-# NOINLINE bar2 #-}
{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 :: Ptr.FunPtr (L -> IO (Ptr.FunPtr (FC.CShort -> IO FC.CInt)))
bar2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_111db9707ae2a631

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar3@
foreign import ccall unsafe "hs_bindgen_00996e2c7c9e793b" hs_bindgen_00996e2c7c9e793b_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar3@
hs_bindgen_00996e2c7c9e793b :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt))))
hs_bindgen_00996e2c7c9e793b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_00996e2c7c9e793b_base

{-# NOINLINE bar3 #-}
{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (S -> IO FC.CInt)))
bar3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00996e2c7c9e793b

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar4@
foreign import ccall unsafe "hs_bindgen_aa46ba685e4693da" hs_bindgen_aa46ba685e4693da_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_bar4@
hs_bindgen_aa46ba685e4693da :: IO (Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I))))
hs_bindgen_aa46ba685e4693da =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_aa46ba685e4693da_base

{-# NOINLINE bar4 #-}
{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 :: Ptr.FunPtr (FC.CLong -> IO (Ptr.FunPtr (FC.CShort -> IO I)))
bar4 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa46ba685e4693da

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz1@
foreign import ccall unsafe "hs_bindgen_86b147342f608069" hs_bindgen_86b147342f608069_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz1@
hs_bindgen_86b147342f608069 :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_86b147342f608069 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_86b147342f608069_base

{-# NOINLINE baz1 #-}
{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_86b147342f608069

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz2@
foreign import ccall unsafe "hs_bindgen_a7194966e4f375df" hs_bindgen_a7194966e4f375df_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz2@
hs_bindgen_a7194966e4f375df :: IO (Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_a7194966e4f375df =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a7194966e4f375df_base

{-# NOINLINE baz2 #-}
{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 :: Ptr.FunPtr (I -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
baz2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a7194966e4f375df

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz3@
foreign import ccall unsafe "hs_bindgen_cd69bb91c3e6d7ea" hs_bindgen_cd69bb91c3e6d7ea_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_baz3@
hs_bindgen_cd69bb91c3e6d7ea :: IO (Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I)))))
hs_bindgen_cd69bb91c3e6d7ea =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cd69bb91c3e6d7ea_base

{-# NOINLINE baz3 #-}
{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 :: Ptr.FunPtr (FC.CInt -> IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 2) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) I))))
baz3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cd69bb91c3e6d7ea

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_d5b5e70d96cc6b8d" hs_bindgen_d5b5e70d96cc6b8d_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_macrosmacro_in_fundecl_Example_get_no_args_no_void@
hs_bindgen_d5b5e70d96cc6b8d :: IO (Ptr.FunPtr (IO I))
hs_bindgen_d5b5e70d96cc6b8d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d5b5e70d96cc6b8d_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: Ptr.FunPtr (IO I)
no_args_no_void =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5b5e70d96cc6b8d
