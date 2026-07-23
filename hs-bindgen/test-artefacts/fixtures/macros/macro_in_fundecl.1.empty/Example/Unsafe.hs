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
  , "char hs_bindgen_8742f64b88435032 ("
  , "  float arg1,"
  , "  char arg2"
  , ")"
  , "{"
  , "  return (quux)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_2a7521a4578b9021 ("
  , "  float arg1,"
  , "  char *arg2"
  , ")"
  , "{"
  , "  return (wam)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_4997a9f97e766872 ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo1)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_8f7161a0cc6544ab ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo2)(arg1, arg2);"
  , "}"
  , "char *hs_bindgen_964a5990350e54ca ("
  , "  float arg1,"
  , "  signed int (*arg2) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  return (foo3)(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_1845a22f2691bd3f ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_2ab30ac8925b7281 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_7a3bc5222b2ae81a ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar3)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_87b07373f703a039 ("
  , "  signed long arg1"
  , ")) ("
  , "  signed short arg1"
  , ")"
  , "{"
  , "  return (bar4)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_4e9b527f6825fd5f ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz1)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_615bc960b75a0bef ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz2)(arg1);"
  , "}"
  , "signed int (*hs_bindgen_414bbfce612b733a ("
  , "  signed int const arg1"
  , "))[2][3]"
  , "{"
  , "  return (baz3)(arg1);"
  , "}"
  , "signed int hs_bindgen_56bb882786815655 (void)"
  , "{"
  , "  return (no_args_no_void)();"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_quux@
foreign import ccall unsafe "hs_bindgen_8742f64b88435032" hs_bindgen_8742f64b88435032_base ::
     Float
  -> BG.Int8
  -> IO BG.Int8

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_quux@
hs_bindgen_8742f64b88435032 ::
     BG.CFloat
  -> BG.CChar
  -> IO BG.CChar
hs_bindgen_8742f64b88435032 =
  BG.fromFFIType hs_bindgen_8742f64b88435032_base

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
quux = hs_bindgen_8742f64b88435032

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_wam@
foreign import ccall unsafe "hs_bindgen_2a7521a4578b9021" hs_bindgen_2a7521a4578b9021_base ::
     Float
  -> BG.Ptr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_wam@
hs_bindgen_2a7521a4578b9021 ::
     BG.CFloat
  -> BG.Ptr BG.CChar
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_2a7521a4578b9021 =
  BG.fromFFIType hs_bindgen_2a7521a4578b9021_base

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
wam = hs_bindgen_2a7521a4578b9021

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo1@
foreign import ccall unsafe "hs_bindgen_4997a9f97e766872" hs_bindgen_4997a9f97e766872_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo1@
hs_bindgen_4997a9f97e766872 ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_4997a9f97e766872 =
  BG.fromFFIType hs_bindgen_4997a9f97e766872_base

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
foo1 = hs_bindgen_4997a9f97e766872

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo2@
foreign import ccall unsafe "hs_bindgen_8f7161a0cc6544ab" hs_bindgen_8f7161a0cc6544ab_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo2@
hs_bindgen_8f7161a0cc6544ab ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_8f7161a0cc6544ab =
  BG.fromFFIType hs_bindgen_8f7161a0cc6544ab_base

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
foo2 = hs_bindgen_8f7161a0cc6544ab

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo3@
foreign import ccall unsafe "hs_bindgen_964a5990350e54ca" hs_bindgen_964a5990350e54ca_base ::
     Float
  -> BG.FunPtr BG.Void
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_foo3@
hs_bindgen_964a5990350e54ca ::
     BG.CFloat
  -> BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO (BG.Ptr BG.CChar)
hs_bindgen_964a5990350e54ca =
  BG.fromFFIType hs_bindgen_964a5990350e54ca_base

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
foo3 = hs_bindgen_964a5990350e54ca

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar1@
foreign import ccall unsafe "hs_bindgen_1845a22f2691bd3f" hs_bindgen_1845a22f2691bd3f_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar1@
hs_bindgen_1845a22f2691bd3f ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_1845a22f2691bd3f =
  BG.fromFFIType hs_bindgen_1845a22f2691bd3f_base

{-| __C declaration:__ @bar1@

    __defined at:__ @macros\/macro_in_fundecl.h 21:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar1 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar1 = hs_bindgen_1845a22f2691bd3f

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar2@
foreign import ccall unsafe "hs_bindgen_2ab30ac8925b7281" hs_bindgen_2ab30ac8925b7281_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar2@
hs_bindgen_2ab30ac8925b7281 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_2ab30ac8925b7281 =
  BG.fromFFIType hs_bindgen_2ab30ac8925b7281_base

{-| __C declaration:__ @bar2@

    __defined at:__ @macros\/macro_in_fundecl.h 22:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar2 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar2 = hs_bindgen_2ab30ac8925b7281

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar3@
foreign import ccall unsafe "hs_bindgen_7a3bc5222b2ae81a" hs_bindgen_7a3bc5222b2ae81a_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar3@
hs_bindgen_7a3bc5222b2ae81a ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_7a3bc5222b2ae81a =
  BG.fromFFIType hs_bindgen_7a3bc5222b2ae81a_base

{-| __C declaration:__ @bar3@

    __defined at:__ @macros\/macro_in_fundecl.h 23:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar3 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar3 = hs_bindgen_7a3bc5222b2ae81a

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar4@
foreign import ccall unsafe "hs_bindgen_87b07373f703a039" hs_bindgen_87b07373f703a039_base ::
     BG.Int64
  -> IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_bar4@
hs_bindgen_87b07373f703a039 ::
     BG.CLong
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
hs_bindgen_87b07373f703a039 =
  BG.fromFFIType hs_bindgen_87b07373f703a039_base

{-| __C declaration:__ @bar4@

    __defined at:__ @macros\/macro_in_fundecl.h 24:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
bar4 ::
     BG.CLong
     -- ^ __C declaration:__ @x@
  -> IO (BG.FunPtr (BG.CShort -> IO BG.CInt))
bar4 = hs_bindgen_87b07373f703a039

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz1@
foreign import ccall unsafe "hs_bindgen_4e9b527f6825fd5f" hs_bindgen_4e9b527f6825fd5f_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz1@
hs_bindgen_4e9b527f6825fd5f ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_4e9b527f6825fd5f =
  BG.fromFFIType hs_bindgen_4e9b527f6825fd5f_base

{-| __C declaration:__ @baz1@

    __defined at:__ @macros\/macro_in_fundecl.h 27:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz1 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz1 = hs_bindgen_4e9b527f6825fd5f

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz2@
foreign import ccall unsafe "hs_bindgen_615bc960b75a0bef" hs_bindgen_615bc960b75a0bef_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz2@
hs_bindgen_615bc960b75a0bef ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_615bc960b75a0bef =
  BG.fromFFIType hs_bindgen_615bc960b75a0bef_base

{-| __C declaration:__ @baz2@

    __defined at:__ @macros\/macro_in_fundecl.h 35:7@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz2 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz2 = hs_bindgen_615bc960b75a0bef

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz3@
foreign import ccall unsafe "hs_bindgen_414bbfce612b733a" hs_bindgen_414bbfce612b733a_base ::
     BG.Int32
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_baz3@
hs_bindgen_414bbfce612b733a ::
     BG.CInt
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
hs_bindgen_414bbfce612b733a =
  BG.fromFFIType hs_bindgen_414bbfce612b733a_base

{-| __C declaration:__ @baz3@

    __defined at:__ @macros\/macro_in_fundecl.h 43:5@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
baz3 ::
     BG.CInt
     -- ^ __C declaration:__ @i@
  -> IO (BG.Ptr (CA.ConstantArray 2 (CA.ConstantArray 3 BG.CInt)))
baz3 = hs_bindgen_414bbfce612b733a

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_no_args_no_void@
foreign import ccall unsafe "hs_bindgen_56bb882786815655" hs_bindgen_56bb882786815655_base ::
     IO BG.Int32

-- __unique:__ @test_macrosmacro_in_fundecl_1_empt_Example_Unsafe_no_args_no_void@
hs_bindgen_56bb882786815655 :: IO BG.CInt
hs_bindgen_56bb882786815655 =
  BG.fromFFIType hs_bindgen_56bb882786815655_base

{-| __C declaration:__ @no_args_no_void@

    __defined at:__ @macros\/macro_in_fundecl.h 53:3@

    __exported by:__ @macros\/macro_in_fundecl.h@
-}
no_args_no_void :: IO BG.CInt
no_args_no_void = hs_bindgen_56bb882786815655
