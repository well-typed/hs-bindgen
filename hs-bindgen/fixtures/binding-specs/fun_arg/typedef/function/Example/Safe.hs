{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  foo(arg1);"
  , "}"
  , "void hs_bindgen_392e3092a2681c13 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(arg1);"
  , "}"
  , "void hs_bindgen_6011faf8531be4fa ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(arg1);"
  , "}"
  , "void hs_bindgen_40a50b8e6ac3b09d ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(arg1);"
  , "}"
  , "void hs_bindgen_32b4f35bf27a4bf8 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(arg1);"
  , "}"
  , "void hs_bindgen_6e59183c0a861d01 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(arg1);"
  , "}"
  , "void hs_bindgen_b80066b7dbab19ae ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  bar(arg1);"
  , "}"
  , "void hs_bindgen_3e2a7974766f255b ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  barA(arg1);"
  , "}"
  , "void hs_bindgen_2e70e27c91ce1929 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  barB(arg1);"
  , "}"
  , "void hs_bindgen_4014a425c2b8c6e7 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  barC(arg1);"
  , "}"
  , "void hs_bindgen_c0263fea7e0285b6 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  barD(arg1);"
  , "}"
  , "void hs_bindgen_431be79cc08aeb21 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  barE(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  RIP.fromFFIType hs_bindgen_99bb90e6d7637d2c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
foo ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_99bb90e6d7637d2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
hs_bindgen_392e3092a2681c13 ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_392e3092a2681c13 =
  RIP.fromFFIType hs_bindgen_392e3092a2681c13_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_392e3092a2681c13

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
hs_bindgen_6011faf8531be4fa ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_6011faf8531be4fa =
  RIP.fromFFIType hs_bindgen_6011faf8531be4fa_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6011faf8531be4fa

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
hs_bindgen_40a50b8e6ac3b09d ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_40a50b8e6ac3b09d =
  RIP.fromFFIType hs_bindgen_40a50b8e6ac3b09d_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_40a50b8e6ac3b09d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
hs_bindgen_32b4f35bf27a4bf8 ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_32b4f35bf27a4bf8 =
  RIP.fromFFIType hs_bindgen_32b4f35bf27a4bf8_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_32b4f35bf27a4bf8

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
hs_bindgen_6e59183c0a861d01 ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_6e59183c0a861d01 =
  RIP.fromFFIType hs_bindgen_6e59183c0a861d01_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_6e59183c0a861d01

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_b80066b7dbab19ae" hs_bindgen_b80066b7dbab19ae_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
hs_bindgen_b80066b7dbab19ae ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
  -> IO ()
hs_bindgen_b80066b7dbab19ae =
  RIP.fromFFIType hs_bindgen_b80066b7dbab19ae_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 28:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
bar ::
     RIP.FunPtr (RIP.CInt -> IO RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_b80066b7dbab19ae

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_3e2a7974766f255b" hs_bindgen_3e2a7974766f255b_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barA@
hs_bindgen_3e2a7974766f255b ::
     RIP.FunPtr A
  -> IO ()
hs_bindgen_3e2a7974766f255b =
  RIP.fromFFIType hs_bindgen_3e2a7974766f255b_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 30:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barA ::
     RIP.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_3e2a7974766f255b

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_2e70e27c91ce1929" hs_bindgen_2e70e27c91ce1929_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barB@
hs_bindgen_2e70e27c91ce1929 ::
     RIP.FunPtr B
  -> IO ()
hs_bindgen_2e70e27c91ce1929 =
  RIP.fromFFIType hs_bindgen_2e70e27c91ce1929_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barB ::
     RIP.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_2e70e27c91ce1929

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barC@
foreign import ccall safe "hs_bindgen_4014a425c2b8c6e7" hs_bindgen_4014a425c2b8c6e7_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barC@
hs_bindgen_4014a425c2b8c6e7 ::
     RIP.FunPtr M.C
  -> IO ()
hs_bindgen_4014a425c2b8c6e7 =
  RIP.fromFFIType hs_bindgen_4014a425c2b8c6e7_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barC ::
     RIP.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_4014a425c2b8c6e7

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barD@
foreign import ccall safe "hs_bindgen_c0263fea7e0285b6" hs_bindgen_c0263fea7e0285b6_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barD@
hs_bindgen_c0263fea7e0285b6 ::
     RIP.FunPtr M.D
  -> IO ()
hs_bindgen_c0263fea7e0285b6 =
  RIP.fromFFIType hs_bindgen_c0263fea7e0285b6_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barD ::
     RIP.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_c0263fea7e0285b6

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barE@
foreign import ccall safe "hs_bindgen_431be79cc08aeb21" hs_bindgen_431be79cc08aeb21_base ::
     RIP.FunPtr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barE@
hs_bindgen_431be79cc08aeb21 ::
     RIP.FunPtr E
  -> IO ()
hs_bindgen_431be79cc08aeb21 =
  RIP.fromFFIType hs_bindgen_431be79cc08aeb21_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barE ::
     RIP.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_431be79cc08aeb21
