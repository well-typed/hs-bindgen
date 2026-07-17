{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.foo
    , Example.Safe.fooA
    , Example.Safe.fooB
    , Example.Safe.fooC
    , Example.Safe.fooD
    , Example.Safe.fooE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/struct.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  (foo)(*arg1);"
  , "}"
  , "void hs_bindgen_392e3092a2681c13 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(*arg1);"
  , "}"
  , "void hs_bindgen_6011faf8531be4fa ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(*arg1);"
  , "}"
  , "void hs_bindgen_40a50b8e6ac3b09d ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(*arg1);"
  , "}"
  , "void hs_bindgen_32b4f35bf27a4bf8 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(*arg1);"
  , "}"
  , "void hs_bindgen_6e59183c0a861d01 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     BG.Ptr MyStruct
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  BG.fromFFIType hs_bindgen_99bb90e6d7637d2c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_99bb90e6d7637d2c x1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
hs_bindgen_392e3092a2681c13 ::
     BG.Ptr A
  -> IO ()
hs_bindgen_392e3092a2681c13 =
  BG.fromFFIType hs_bindgen_392e3092a2681c13_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_392e3092a2681c13 x1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
hs_bindgen_6011faf8531be4fa ::
     BG.Ptr B
  -> IO ()
hs_bindgen_6011faf8531be4fa =
  BG.fromFFIType hs_bindgen_6011faf8531be4fa_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_6011faf8531be4fa x1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
hs_bindgen_40a50b8e6ac3b09d ::
     BG.Ptr M.C
  -> IO ()
hs_bindgen_40a50b8e6ac3b09d =
  BG.fromFFIType hs_bindgen_40a50b8e6ac3b09d_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_40a50b8e6ac3b09d x1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
hs_bindgen_32b4f35bf27a4bf8 ::
     BG.Ptr M.D
  -> IO ()
hs_bindgen_32b4f35bf27a4bf8 =
  BG.fromFFIType hs_bindgen_32b4f35bf27a4bf8_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_32b4f35bf27a4bf8 x1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
hs_bindgen_6e59183c0a861d01 ::
     BG.Ptr E
  -> IO ()
hs_bindgen_6e59183c0a861d01 =
  BG.fromFFIType hs_bindgen_6e59183c0a861d01_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/struct.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/struct.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_6e59183c0a861d01 x1)
