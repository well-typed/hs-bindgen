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

import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/array.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (foo)(arg1);"
  , "}"
  , "void hs_bindgen_392e3092a2681c13 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooA)(arg1);"
  , "}"
  , "void hs_bindgen_6011faf8531be4fa ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooB)(arg1);"
  , "}"
  , "void hs_bindgen_40a50b8e6ac3b09d ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooC)(arg1);"
  , "}"
  , "void hs_bindgen_32b4f35bf27a4bf8 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooD)(arg1);"
  , "}"
  , "void hs_bindgen_6e59183c0a861d01 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  (fooE)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  BG.fromFFIType hs_bindgen_99bb90e6d7637d2c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
foo ::
     BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt))
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_99bb90e6d7637d2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
hs_bindgen_392e3092a2681c13 ::
     BG.Ptr (IsA.Elem A)
  -> IO ()
hs_bindgen_392e3092a2681c13 =
  BG.fromFFIType hs_bindgen_392e3092a2681c13_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooA ::
     BG.Ptr (IsA.Elem A)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_392e3092a2681c13

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
hs_bindgen_6011faf8531be4fa ::
     BG.Ptr (IsA.Elem B)
  -> IO ()
hs_bindgen_6011faf8531be4fa =
  BG.fromFFIType hs_bindgen_6011faf8531be4fa_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooB ::
     BG.Ptr (IsA.Elem B)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6011faf8531be4fa

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
hs_bindgen_40a50b8e6ac3b09d ::
     BG.Ptr (IsA.Elem M.C)
  -> IO ()
hs_bindgen_40a50b8e6ac3b09d =
  BG.fromFFIType hs_bindgen_40a50b8e6ac3b09d_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooC ::
     BG.Ptr (IsA.Elem M.C)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_40a50b8e6ac3b09d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
hs_bindgen_32b4f35bf27a4bf8 ::
     BG.Ptr (IsA.Elem M.D)
  -> IO ()
hs_bindgen_32b4f35bf27a4bf8 =
  BG.fromFFIType hs_bindgen_32b4f35bf27a4bf8_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooD ::
     BG.Ptr (IsA.Elem M.D)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_32b4f35bf27a4bf8

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
hs_bindgen_6e59183c0a861d01 ::
     BG.Ptr (IsA.Elem E)
  -> IO ()
hs_bindgen_6e59183c0a861d01 =
  BG.fromFFIType hs_bindgen_6e59183c0a861d01_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooE ::
     BG.Ptr (IsA.Elem E)
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_6e59183c0a861d01
