{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.quux
    , Example.FunPtr.wam
    , Example.FunPtr.foo1
    , Example.FunPtr.foo2
    , Example.FunPtr.foo3
    , Example.FunPtr.bar1
    , Example.FunPtr.bar2
    , Example.FunPtr.bar3
    , Example.FunPtr.bar4
    , Example.FunPtr.baz1
    , Example.FunPtr.baz2
    , Example.FunPtr.baz3
    , Example.FunPtr.no_args_no_void
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_quux */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_5287e20e5e7b825d (void)) ("
  , "  float arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return &quux;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_wam */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_c99bba48e65f69c2 (void)) ("
  , "  float arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return &wam;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_foo1 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_c6abb6228804eb6f (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_foo2 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_2cd68a8ff1c700b1 (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_foo3 */"
  , "__attribute__ ((const))"
  , "char *(*hs_bindgen_7f22a584414189b4 (void)) ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return &foo3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_bar1 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_c1bdd45feff88132 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_bar2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_db4789e738f22433 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_bar3 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_ce7381c4dae45aa0 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_bar4 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_9fc1ae53f4373826 (void)) ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return &bar4;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_baz1 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_7371729bfd8f41e0 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz1;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_baz2 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_348d64df5f006ad8 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz2;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_baz3 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_d8409f68bb588204 (void)) ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return &baz3;"
  , "}"
  , "/* test_macrosmacro_in_fundecl_1_empt_Example_get_no_args_no_void */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5a80b21813f738d7 (void)) (void)"
  , "{"
  , "  return &no_args_no_void;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_quux@
foreign import ccall unsafe "hs_bindgen_5287e20e5e7b825d" hs_bindgen_5287e20e5e7b825d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_quux@
hs_bindgen_5287e20e5e7b825d :: IO (BG.FunPtr (BG.CFloat -> BG.CChar -> IO BG.CChar))
hs_bindgen_5287e20e5e7b825d =
  BG.fromFFIType hs_bindgen_5287e20e5e7b825d_base

{-# NOINLINE quux #-}
{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux :: BG.FunPtr (BG.CFloat -> BG.CChar -> IO BG.CChar)
quux = BG.unsafePerformIO hs_bindgen_5287e20e5e7b825d

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_wam@
foreign import ccall unsafe "hs_bindgen_c99bba48e65f69c2" hs_bindgen_c99bba48e65f69c2_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_wam@
hs_bindgen_c99bba48e65f69c2 :: IO (BG.FunPtr (BG.CFloat -> BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar)))
hs_bindgen_c99bba48e65f69c2 =
  BG.fromFFIType hs_bindgen_c99bba48e65f69c2_base

{-# NOINLINE wam #-}
{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam :: BG.FunPtr (BG.CFloat -> BG.Ptr BG.CChar -> IO (BG.Ptr BG.CChar))
wam = BG.unsafePerformIO hs_bindgen_c99bba48e65f69c2

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo1@
foreign import ccall unsafe "hs_bindgen_c6abb6228804eb6f" hs_bindgen_c6abb6228804eb6f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo1@
hs_bindgen_c6abb6228804eb6f :: IO (BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_c6abb6228804eb6f =
  BG.fromFFIType hs_bindgen_c6abb6228804eb6f_base

{-# NOINLINE foo1 #-}
{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 :: BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar))
foo1 = BG.unsafePerformIO hs_bindgen_c6abb6228804eb6f

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo2@
foreign import ccall unsafe "hs_bindgen_2cd68a8ff1c700b1" hs_bindgen_2cd68a8ff1c700b1_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo2@
hs_bindgen_2cd68a8ff1c700b1 :: IO (BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_2cd68a8ff1c700b1 =
  BG.fromFFIType hs_bindgen_2cd68a8ff1c700b1_base

{-# NOINLINE foo2 #-}
{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 :: BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar))
foo2 = BG.unsafePerformIO hs_bindgen_2cd68a8ff1c700b1

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo3@
foreign import ccall unsafe "hs_bindgen_7f22a584414189b4" hs_bindgen_7f22a584414189b4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_foo3@
hs_bindgen_7f22a584414189b4 :: IO (BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar)))
hs_bindgen_7f22a584414189b4 =
  BG.fromFFIType hs_bindgen_7f22a584414189b4_base

{-# NOINLINE foo3 #-}
{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 :: BG.FunPtr (BG.CFloat -> BG.FunPtr (BG.CInt -> IO BG.CInt) -> IO (BG.Ptr BG.CChar))
foo3 = BG.unsafePerformIO hs_bindgen_7f22a584414189b4

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar1@
foreign import ccall unsafe "hs_bindgen_c1bdd45feff88132" hs_bindgen_c1bdd45feff88132_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar1@
hs_bindgen_c1bdd45feff88132 :: IO (BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))))
hs_bindgen_c1bdd45feff88132 =
  BG.fromFFIType hs_bindgen_c1bdd45feff88132_base

{-# NOINLINE bar1 #-}
{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 :: BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt)))
bar1 = BG.unsafePerformIO hs_bindgen_c1bdd45feff88132

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar2@
foreign import ccall unsafe "hs_bindgen_db4789e738f22433" hs_bindgen_db4789e738f22433_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar2@
hs_bindgen_db4789e738f22433 :: IO (BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))))
hs_bindgen_db4789e738f22433 =
  BG.fromFFIType hs_bindgen_db4789e738f22433_base

{-# NOINLINE bar2 #-}
{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 :: BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt)))
bar2 = BG.unsafePerformIO hs_bindgen_db4789e738f22433

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar3@
foreign import ccall unsafe "hs_bindgen_ce7381c4dae45aa0" hs_bindgen_ce7381c4dae45aa0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar3@
hs_bindgen_ce7381c4dae45aa0 :: IO (BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))))
hs_bindgen_ce7381c4dae45aa0 =
  BG.fromFFIType hs_bindgen_ce7381c4dae45aa0_base

{-# NOINLINE bar3 #-}
{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 :: BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt)))
bar3 = BG.unsafePerformIO hs_bindgen_ce7381c4dae45aa0

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar4@
foreign import ccall unsafe "hs_bindgen_9fc1ae53f4373826" hs_bindgen_9fc1ae53f4373826_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_bar4@
hs_bindgen_9fc1ae53f4373826 :: IO (BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))))
hs_bindgen_9fc1ae53f4373826 =
  BG.fromFFIType hs_bindgen_9fc1ae53f4373826_base

{-# NOINLINE bar4 #-}
{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 :: BG.FunPtr (BG.CLong -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt)))
bar4 = BG.unsafePerformIO hs_bindgen_9fc1ae53f4373826

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz1@
foreign import ccall unsafe "hs_bindgen_7371729bfd8f41e0" hs_bindgen_7371729bfd8f41e0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz1@
hs_bindgen_7371729bfd8f41e0 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))))
hs_bindgen_7371729bfd8f41e0 =
  BG.fromFFIType hs_bindgen_7371729bfd8f41e0_base

{-# NOINLINE baz1 #-}
{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt))))
baz1 = BG.unsafePerformIO hs_bindgen_7371729bfd8f41e0

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz2@
foreign import ccall unsafe "hs_bindgen_348d64df5f006ad8" hs_bindgen_348d64df5f006ad8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz2@
hs_bindgen_348d64df5f006ad8 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))))
hs_bindgen_348d64df5f006ad8 =
  BG.fromFFIType hs_bindgen_348d64df5f006ad8_base

{-# NOINLINE baz2 #-}
{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt))))
baz2 = BG.unsafePerformIO hs_bindgen_348d64df5f006ad8

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz3@
foreign import ccall unsafe "hs_bindgen_d8409f68bb588204" hs_bindgen_d8409f68bb588204_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_baz3@
hs_bindgen_d8409f68bb588204 :: IO (BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))))
hs_bindgen_d8409f68bb588204 =
  BG.fromFFIType hs_bindgen_d8409f68bb588204_base

{-# NOINLINE baz3 #-}
{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 :: BG.FunPtr (BG.CInt -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt))))
baz3 = BG.unsafePerformIO hs_bindgen_d8409f68bb588204

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_5a80b21813f738d7" hs_bindgen_5a80b21813f738d7_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_get_no_args_no_void@
hs_bindgen_5a80b21813f738d7 :: IO (BG.FunPtr (IO BG.CInt))
hs_bindgen_5a80b21813f738d7 =
  BG.fromFFIType hs_bindgen_5a80b21813f738d7_base

{-# NOINLINE no_args_no_void #-}
{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: BG.FunPtr (IO BG.CInt)
no_args_no_void =
  BG.unsafePerformIO hs_bindgen_5a80b21813f738d7
