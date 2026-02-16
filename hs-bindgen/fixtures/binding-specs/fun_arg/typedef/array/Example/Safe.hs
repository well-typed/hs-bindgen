{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified M
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/typedef/array.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  signed int (*arg1)[]"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_392e3092a2681c13 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_6011faf8531be4fa ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  , "void hs_bindgen_40a50b8e6ac3b09d ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  fooC(*arg1);"
  , "}"
  , "void hs_bindgen_32b4f35bf27a4bf8 ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  fooD(*arg1);"
  , "}"
  , "void hs_bindgen_6e59183c0a861d01 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  fooE(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_foo@
hs_bindgen_99bb90e6d7637d2c ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
  -> IO ()
hs_bindgen_99bb90e6d7637d2c =
  RIP.fromFFIType hs_bindgen_99bb90e6d7637d2c_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 4:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
foo ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @x@
  -> IO ()
foo = hs_bindgen_99bb90e6d7637d2c

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
hs_bindgen_392e3092a2681c13 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_392e3092a2681c13 =
  RIP.fromFFIType hs_bindgen_392e3092a2681c13_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooA ::
     RIP.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA = hs_bindgen_392e3092a2681c13

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
hs_bindgen_6011faf8531be4fa ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_6011faf8531be4fa =
  RIP.fromFFIType hs_bindgen_6011faf8531be4fa_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooB ::
     RIP.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB = hs_bindgen_6011faf8531be4fa

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
hs_bindgen_40a50b8e6ac3b09d ::
     RIP.Ptr M.C
  -> IO ()
hs_bindgen_40a50b8e6ac3b09d =
  RIP.fromFFIType hs_bindgen_40a50b8e6ac3b09d_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 21:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooC ::
     RIP.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC = hs_bindgen_40a50b8e6ac3b09d

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
hs_bindgen_32b4f35bf27a4bf8 ::
     RIP.Ptr M.D
  -> IO ()
hs_bindgen_32b4f35bf27a4bf8 =
  RIP.fromFFIType hs_bindgen_32b4f35bf27a4bf8_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 22:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooD ::
     RIP.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD = hs_bindgen_32b4f35bf27a4bf8

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
hs_bindgen_6e59183c0a861d01 ::
     RIP.Ptr E
  -> IO ()
hs_bindgen_6e59183c0a861d01 =
  RIP.fromFFIType hs_bindgen_6e59183c0a861d01_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/array.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/array.h@
-}
fooE ::
     RIP.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE = hs_bindgen_6e59183c0a861d01
