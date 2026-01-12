{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import qualified M
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <binding-specs/fun_arg/typedef/union.h>"
  , "void hs_bindgen_99bb90e6d7637d2c ("
  , "  union MyUnion *arg1"
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
foreign import ccall safe "hs_bindgen_99bb90e6d7637d2c" hs_bindgen_99bb90e6d7637d2c ::
     Ptr.Ptr MyUnion
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_99bb90e6d7637d2c

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
foo ::
     MyUnion
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_99bb90e6d7637d2c y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_392e3092a2681c13" hs_bindgen_392e3092a2681c13 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_392e3092a2681c13

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_392e3092a2681c13 y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_6011faf8531be4fa" hs_bindgen_6011faf8531be4fa ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_6011faf8531be4fa

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_6011faf8531be4fa y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooC@
foreign import ccall safe "hs_bindgen_40a50b8e6ac3b09d" hs_bindgen_40a50b8e6ac3b09d ::
     Ptr.Ptr M.C
  -> IO ()

{-| Pointer-based API for 'fooC'
-}
fooC_wrapper ::
     Ptr.Ptr M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC_wrapper = hs_bindgen_40a50b8e6ac3b09d

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 23:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_40a50b8e6ac3b09d y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooD@
foreign import ccall safe "hs_bindgen_32b4f35bf27a4bf8" hs_bindgen_32b4f35bf27a4bf8 ::
     Ptr.Ptr M.D
  -> IO ()

{-| Pointer-based API for 'fooD'
-}
fooD_wrapper ::
     Ptr.Ptr M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD_wrapper = hs_bindgen_32b4f35bf27a4bf8

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 24:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_32b4f35bf27a4bf8 y1)

-- __unique:__ @test_bindingspecsfun_argtypedef_Example_Safe_fooE@
foreign import ccall safe "hs_bindgen_6e59183c0a861d01" hs_bindgen_6e59183c0a861d01 ::
     Ptr.Ptr E
  -> IO ()

{-| Pointer-based API for 'fooE'
-}
fooE_wrapper ::
     Ptr.Ptr E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE_wrapper = hs_bindgen_6e59183c0a861d01

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/typedef\/union.h 25:6@

    __exported by:__ @binding-specs\/fun_arg\/typedef\/union.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    F.with x0 (\y1 -> hs_bindgen_6e59183c0a861d01 y1)
