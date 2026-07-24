{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.quux
    , Example.Unsafe.wam
    , Example.Unsafe.foo1
    , Example.Unsafe.foo2
    , Example.Unsafe.foo3
    , Example.Unsafe.bar1
    , Example.Unsafe.bar2
    , Example.Unsafe.bar3
    , Example.Unsafe.bar4
    , Example.Unsafe.baz1
    , Example.Unsafe.baz2
    , Example.Unsafe.baz3
    , Example.Unsafe.no_args_no_void
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_in_fundecl.h>"
  , "char hs_bindgen_6b552cf38fe835d9 ("
  , "  float arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_c21f84e027d8887c ("
  , "  float arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (wam)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_2401297c490b2b53 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo1)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_245d9b4b607a0239 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo2)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_f731296a8b75c18c ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo3)(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_9e8e0078d6f5f10d ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_c3dc7bc6e13e4444 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_b10397be2af8a563 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar3)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_3670d1c6cdde814b ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar4)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_9d5573689ffc268e ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_936c3cbcdbf36457 ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_6fdade83f34edbda ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz3)(arg1);"
  , "}"
  , "signed int hs_bindgen_8909620ea33bb4d6 (void)"
  , "{"
  , "  return (no_args_no_void)();"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_6b552cf38fe835d9" hs_bindgen_6b552cf38fe835d9_base ::
     Float
  -> BG.Int8
  -> IO BG.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_quux@
hs_bindgen_6b552cf38fe835d9 ::
     BG.CFloat
  -> BG.CChar
  -> IO BG.CChar
hs_bindgen_6b552cf38fe835d9 =
  BG.fromFFIType hs_bindgen_6b552cf38fe835d9_base

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
quux = hs_bindgen_6b552cf38fe835d9

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_wam@
foreign import ccall unsafe "hs_bindgen_c21f84e027d8887c" hs_bindgen_c21f84e027d8887c_base ::
     Float
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_wam@
hs_bindgen_c21f84e027d8887c ::
     BG.CFloat
  -> BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_c21f84e027d8887c =
  BG.fromFFIType hs_bindgen_c21f84e027d8887c_base

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
wam = hs_bindgen_c21f84e027d8887c

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo1@
foreign import ccall unsafe "hs_bindgen_2401297c490b2b53" hs_bindgen_2401297c490b2b53_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo1@
hs_bindgen_2401297c490b2b53 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_2401297c490b2b53 =
  BG.fromFFIType hs_bindgen_2401297c490b2b53_base

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
foo1 = hs_bindgen_2401297c490b2b53

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo2@
foreign import ccall unsafe "hs_bindgen_245d9b4b607a0239" hs_bindgen_245d9b4b607a0239_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo2@
hs_bindgen_245d9b4b607a0239 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_245d9b4b607a0239 =
  BG.fromFFIType hs_bindgen_245d9b4b607a0239_base

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
foo2 = hs_bindgen_245d9b4b607a0239

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo3@
foreign import ccall unsafe "hs_bindgen_f731296a8b75c18c" hs_bindgen_f731296a8b75c18c_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_foo3@
hs_bindgen_f731296a8b75c18c ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_f731296a8b75c18c =
  BG.fromFFIType hs_bindgen_f731296a8b75c18c_base

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
foo3 = hs_bindgen_f731296a8b75c18c

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar1@
foreign import ccall unsafe "hs_bindgen_9e8e0078d6f5f10d" hs_bindgen_9e8e0078d6f5f10d_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar1@
hs_bindgen_9e8e0078d6f5f10d ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_9e8e0078d6f5f10d =
  BG.fromFFIType hs_bindgen_9e8e0078d6f5f10d_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar1 = hs_bindgen_9e8e0078d6f5f10d

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar2@
foreign import ccall unsafe "hs_bindgen_c3dc7bc6e13e4444" hs_bindgen_c3dc7bc6e13e4444_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar2@
hs_bindgen_c3dc7bc6e13e4444 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_c3dc7bc6e13e4444 =
  BG.fromFFIType hs_bindgen_c3dc7bc6e13e4444_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar2 = hs_bindgen_c3dc7bc6e13e4444

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar3@
foreign import ccall unsafe "hs_bindgen_b10397be2af8a563" hs_bindgen_b10397be2af8a563_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar3@
hs_bindgen_b10397be2af8a563 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_b10397be2af8a563 =
  BG.fromFFIType hs_bindgen_b10397be2af8a563_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar3 = hs_bindgen_b10397be2af8a563

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar4@
foreign import ccall unsafe "hs_bindgen_3670d1c6cdde814b" hs_bindgen_3670d1c6cdde814b_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_bar4@
hs_bindgen_3670d1c6cdde814b ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_3670d1c6cdde814b =
  BG.fromFFIType hs_bindgen_3670d1c6cdde814b_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar4 = hs_bindgen_3670d1c6cdde814b

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz1@
foreign import ccall unsafe "hs_bindgen_9d5573689ffc268e" hs_bindgen_9d5573689ffc268e_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz1@
hs_bindgen_9d5573689ffc268e ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_9d5573689ffc268e =
  BG.fromFFIType hs_bindgen_9d5573689ffc268e_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz1 = hs_bindgen_9d5573689ffc268e

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz2@
foreign import ccall unsafe "hs_bindgen_936c3cbcdbf36457" hs_bindgen_936c3cbcdbf36457_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz2@
hs_bindgen_936c3cbcdbf36457 ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_936c3cbcdbf36457 =
  BG.fromFFIType hs_bindgen_936c3cbcdbf36457_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz2 = hs_bindgen_936c3cbcdbf36457

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz3@
foreign import ccall unsafe "hs_bindgen_6fdade83f34edbda" hs_bindgen_6fdade83f34edbda_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_baz3@
hs_bindgen_6fdade83f34edbda ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_6fdade83f34edbda =
  BG.fromFFIType hs_bindgen_6fdade83f34edbda_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz3 = hs_bindgen_6fdade83f34edbda

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_8909620ea33bb4d6" hs_bindgen_8909620ea33bb4d6_base ::
     IO BG.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_2_raw_Example_Unsafe_no_args_no_void@
hs_bindgen_8909620ea33bb4d6 :: IO BG.CInt
hs_bindgen_8909620ea33bb4d6 =
  BG.fromFFIType hs_bindgen_8909620ea33bb4d6_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO BG.CInt
no_args_no_void = hs_bindgen_8909620ea33bb4d6
