{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.fooA
    , Example.Safe.fooB
    , Example.Safe.fooC
    , Example.Safe.fooD
    , Example.Safe.fooE
    , Example.Safe.bar
    , Example.Safe.barA
    , Example.Safe.barB
    , Example.Safe.barC
    , Example.Safe.barD
    , Example.Safe.barE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/function.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_392e3092a2681c13 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_6011faf8531be4fa ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_40a50b8e6ac3b09d ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_32b4f35bf27a4bf8 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_6e59183c0a861d01 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  , "void hs_bindgen_b80066b7dbab19ae ("
  , "  signed int (*arg1) ("
  , "  signed int arg1"
  , ")"
  , ")"
  , "{"
  , "  (bar)(arg1);"
  , "}"
  , "void hs_bindgen_3e2a7974766f255b ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (barA)(arg1);"
  , "}"
  , "void hs_bindgen_2e70e27c91ce1929 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (barB)(arg1);"
  , "}"
  , "void hs_bindgen_4014a425c2b8c6e7 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (barC)(arg1);"
  , "}"
  , "void hs_bindgen_c0263fea7e0285b6 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (barD)(arg1);"
  , "}"
  , "void hs_bindgen_431be79cc08aeb21 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (barE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  BG.fromFFIType hs_bindgen_99bb90e6d7637d2c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
foo ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_99bb90e6d7637d2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
hs_bindgen_392e3092a2681c13 ::
     BG.FunPtr A
  -> IO ()
hs_bindgen_392e3092a2681c13 =
  BG.fromFFIType hs_bindgen_392e3092a2681c13_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_392e3092a2681c13

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
hs_bindgen_6011faf8531be4fa ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_6011faf8531be4fa =
  BG.fromFFIType hs_bindgen_6011faf8531be4fa_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6011faf8531be4fa

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
hs_bindgen_40a50b8e6ac3b09d ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_40a50b8e6ac3b09d =
  BG.fromFFIType hs_bindgen_40a50b8e6ac3b09d_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_40a50b8e6ac3b09d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
hs_bindgen_32b4f35bf27a4bf8 ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_32b4f35bf27a4bf8 =
  BG.fromFFIType hs_bindgen_32b4f35bf27a4bf8_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_32b4f35bf27a4bf8

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
hs_bindgen_6e59183c0a861d01 ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_6e59183c0a861d01 =
  BG.fromFFIType hs_bindgen_6e59183c0a861d01_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
fooE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_6e59183c0a861d01

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
foreign import ccall safe "hs_bindgen_b80066b7dbab19ae" hs_bindgen_b80066b7dbab19ae_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_bar@
hs_bindgen_b80066b7dbab19ae ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
  -> IO ()
hs_bindgen_b80066b7dbab19ae =
  BG.fromFFIType hs_bindgen_b80066b7dbab19ae_base

{-| __C declaration:__ @bar@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 28:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
bar ::
     BG.FunPtr (BG.CInt -> IO BG.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
bar = hs_bindgen_b80066b7dbab19ae

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barA@
foreign import ccall safe "hs_bindgen_3e2a7974766f255b" hs_bindgen_3e2a7974766f255b_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barA@
hs_bindgen_3e2a7974766f255b ::
     BG.FunPtr A
  -> IO ()
hs_bindgen_3e2a7974766f255b =
  BG.fromFFIType hs_bindgen_3e2a7974766f255b_base

{-| __C declaration:__ @barA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 30:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barA ::
     BG.FunPtr A
     -- ^ __C declaration:__ @x@
  -> IO ()
barA = hs_bindgen_3e2a7974766f255b

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barB@
foreign import ccall safe "hs_bindgen_2e70e27c91ce1929" hs_bindgen_2e70e27c91ce1929_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barB@
hs_bindgen_2e70e27c91ce1929 ::
     BG.FunPtr B
  -> IO ()
hs_bindgen_2e70e27c91ce1929 =
  BG.fromFFIType hs_bindgen_2e70e27c91ce1929_base

{-| __C declaration:__ @barB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 31:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barB ::
     BG.FunPtr B
     -- ^ __C declaration:__ @x@
  -> IO ()
barB = hs_bindgen_2e70e27c91ce1929

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barC@
foreign import ccall safe "hs_bindgen_4014a425c2b8c6e7" hs_bindgen_4014a425c2b8c6e7_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barC@
hs_bindgen_4014a425c2b8c6e7 ::
     BG.FunPtr M.C
  -> IO ()
hs_bindgen_4014a425c2b8c6e7 =
  BG.fromFFIType hs_bindgen_4014a425c2b8c6e7_base

{-| __C declaration:__ @barC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 33:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barC ::
     BG.FunPtr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
barC = hs_bindgen_4014a425c2b8c6e7

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barD@
foreign import ccall safe "hs_bindgen_c0263fea7e0285b6" hs_bindgen_c0263fea7e0285b6_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barD@
hs_bindgen_c0263fea7e0285b6 ::
     BG.FunPtr M.D
  -> IO ()
hs_bindgen_c0263fea7e0285b6 =
  BG.fromFFIType hs_bindgen_c0263fea7e0285b6_base

{-| __C declaration:__ @barD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 34:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barD ::
     BG.FunPtr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
barD = hs_bindgen_c0263fea7e0285b6

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barE@
foreign import ccall safe "hs_bindgen_431be79cc08aeb21" hs_bindgen_431be79cc08aeb21_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_barE@
hs_bindgen_431be79cc08aeb21 ::
     BG.FunPtr E
  -> IO ()
hs_bindgen_431be79cc08aeb21 =
  BG.fromFFIType hs_bindgen_431be79cc08aeb21_base

{-| __C declaration:__ @barE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/function.h 35:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/function.h@
-}
barE ::
     BG.FunPtr E
     -- ^ __C declaration:__ @x@
  -> IO ()
barE = hs_bindgen_431be79cc08aeb21
