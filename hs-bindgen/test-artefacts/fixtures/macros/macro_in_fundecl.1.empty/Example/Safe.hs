{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.quux
    , Example.Safe.wam
    , Example.Safe.foo1
    , Example.Safe.foo2
    , Example.Safe.foo3
    , Example.Safe.bar1
    , Example.Safe.bar2
    , Example.Safe.bar3
    , Example.Safe.bar4
    , Example.Safe.baz1
    , Example.Safe.baz2
    , Example.Safe.baz3
    , Example.Safe.no_args_no_void
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_0d589d05ef4464f1 ("
  , "  float arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_5e34a9a021a16ef2 ("
  , "  float arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (wam)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_a653f9965418fe35 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo1)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_27d5c3bf56ffe127 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo2)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_28f84ffb010a241f ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo3)(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_bd2ce04af55e7267 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_71466c42e4144c66 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_158e79501bca5645 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar3)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_114a38b3ab9e70ba ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar4)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_3bbe244ce8f2af84 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_1d115779ef74db4e ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_3609a59984f4f6c6 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz3)(arg1);"
  , "}"
  , "signed int hs_bindgen_0d9adddf37d97870 (void)"
  , "{"
  , "  return (no_args_no_void)();"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_quux@
foreign import ccall safe "hs_bindgen_0d589d05ef4464f1" hs_bindgen_0d589d05ef4464f1_base ::
     Float
  -> BG.Int8
  -> IO BG.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_quux@
hs_bindgen_0d589d05ef4464f1 ::
     BG.CFloat
  -> BG.CChar
  -> IO BG.CChar
hs_bindgen_0d589d05ef4464f1 =
  BG.fromFFIType hs_bindgen_0d589d05ef4464f1_base

{-| __C declaration:__ @quux@

    __defined at:__ @macros\/macro_in_fundecl.h 12:6@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
quux ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.CChar
     -- ^ __C declaration:__ @y@
  -> IO BG.CChar
quux = hs_bindgen_0d589d05ef4464f1

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_wam@
foreign import ccall safe "hs_bindgen_5e34a9a021a16ef2" hs_bindgen_5e34a9a021a16ef2_base ::
     Float
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_wam@
hs_bindgen_5e34a9a021a16ef2 ::
     BG.CFloat
  -> BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_5e34a9a021a16ef2 =
  BG.fromFFIType hs_bindgen_5e34a9a021a16ef2_base

{-| __C declaration:__ @wam@

    __defined at:__ @macros\/macro_in_fundecl.h 13:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
wam ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.Ptr BG.CChar
     -- ^ __C declaration:__ @y@
  -> IO (BG.Ptr BG.CChar)
wam = hs_bindgen_5e34a9a021a16ef2

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo1@
foreign import ccall safe "hs_bindgen_a653f9965418fe35" hs_bindgen_a653f9965418fe35_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo1@
hs_bindgen_a653f9965418fe35 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_a653f9965418fe35 =
  BG.fromFFIType hs_bindgen_a653f9965418fe35_base

{-| __C declaration:__ @foo1@

    __defined at:__ @macros\/macro_in_fundecl.h 16:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo1 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo1 = hs_bindgen_a653f9965418fe35

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo2@
foreign import ccall safe "hs_bindgen_27d5c3bf56ffe127" hs_bindgen_27d5c3bf56ffe127_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo2@
hs_bindgen_27d5c3bf56ffe127 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_27d5c3bf56ffe127 =
  BG.fromFFIType hs_bindgen_27d5c3bf56ffe127_base

{-| __C declaration:__ @foo2@

    __defined at:__ @macros\/macro_in_fundecl.h 17:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo2 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo2 = hs_bindgen_27d5c3bf56ffe127

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo3@
foreign import ccall safe "hs_bindgen_28f84ffb010a241f" hs_bindgen_28f84ffb010a241f_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_foo3@
hs_bindgen_28f84ffb010a241f ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_28f84ffb010a241f =
  BG.fromFFIType hs_bindgen_28f84ffb010a241f_base

{-| __C declaration:__ @foo3@

    __defined at:__ @macros\/macro_in_fundecl.h 18:4@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
foo3 ::
     BG.CFloat
     -- ^ __C declaration:__ @x@
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @g@
  -> IO (BG.Ptr BG.CChar)
foo3 = hs_bindgen_28f84ffb010a241f

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar1@
foreign import ccall safe "hs_bindgen_bd2ce04af55e7267" hs_bindgen_bd2ce04af55e7267_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar1@
hs_bindgen_bd2ce04af55e7267 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_bd2ce04af55e7267 =
  BG.fromFFIType hs_bindgen_bd2ce04af55e7267_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar1 = hs_bindgen_bd2ce04af55e7267

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar2@
foreign import ccall safe "hs_bindgen_71466c42e4144c66" hs_bindgen_71466c42e4144c66_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar2@
hs_bindgen_71466c42e4144c66 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_71466c42e4144c66 =
  BG.fromFFIType hs_bindgen_71466c42e4144c66_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar2 = hs_bindgen_71466c42e4144c66

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar3@
foreign import ccall safe "hs_bindgen_158e79501bca5645" hs_bindgen_158e79501bca5645_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar3@
hs_bindgen_158e79501bca5645 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_158e79501bca5645 =
  BG.fromFFIType hs_bindgen_158e79501bca5645_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar3 = hs_bindgen_158e79501bca5645

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar4@
foreign import ccall safe "hs_bindgen_114a38b3ab9e70ba" hs_bindgen_114a38b3ab9e70ba_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_bar4@
hs_bindgen_114a38b3ab9e70ba ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_114a38b3ab9e70ba =
  BG.fromFFIType hs_bindgen_114a38b3ab9e70ba_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar4 = hs_bindgen_114a38b3ab9e70ba

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz1@
foreign import ccall safe "hs_bindgen_3bbe244ce8f2af84" hs_bindgen_3bbe244ce8f2af84_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz1@
hs_bindgen_3bbe244ce8f2af84 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_3bbe244ce8f2af84 =
  BG.fromFFIType hs_bindgen_3bbe244ce8f2af84_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz1 = hs_bindgen_3bbe244ce8f2af84

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz2@
foreign import ccall safe "hs_bindgen_1d115779ef74db4e" hs_bindgen_1d115779ef74db4e_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz2@
hs_bindgen_1d115779ef74db4e ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_1d115779ef74db4e =
  BG.fromFFIType hs_bindgen_1d115779ef74db4e_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz2 = hs_bindgen_1d115779ef74db4e

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz3@
foreign import ccall safe "hs_bindgen_3609a59984f4f6c6" hs_bindgen_3609a59984f4f6c6_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_baz3@
hs_bindgen_3609a59984f4f6c6 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_3609a59984f4f6c6 =
  BG.fromFFIType hs_bindgen_3609a59984f4f6c6_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz3 = hs_bindgen_3609a59984f4f6c6

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_no_args_no_void@
foreign import ccall safe "hs_bindgen_0d9adddf37d97870" hs_bindgen_0d9adddf37d97870_base ::
     IO BG.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Safe_no_args_no_void@
hs_bindgen_0d9adddf37d97870 :: IO BG.CInt
hs_bindgen_0d9adddf37d97870 =
  BG.fromFFIType hs_bindgen_0d9adddf37d97870_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO BG.CInt
no_args_no_void = hs_bindgen_0d9adddf37d97870
