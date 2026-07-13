{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.foo
    , Example.Unsafe.fooA
    , Example.Unsafe.fooB
    , Example.Unsafe.fooC
    , Example.Unsafe.fooD
    , Example.Unsafe.fooE
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "void hs_bindgen_9dbeca9fa307eee9 ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  (foo)(*arg1);"
  , "}"
  , "void hs_bindgen_fb94ee6f22de1d89 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  (fooA)(*arg1);"
  , "}"
  , "void hs_bindgen_f8f83a7090687b7e ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  (fooB)(*arg1);"
  , "}"
  , "void hs_bindgen_f1d253a32d18ddb1 ("
  , "  C *arg1"
  , ")"
  , "{"
  , "  (fooC)(*arg1);"
  , "}"
  , "void hs_bindgen_2ee0831a468707cf ("
  , "  D *arg1"
  , ")"
  , "{"
  , "  (fooD)(*arg1);"
  , "}"
  , "void hs_bindgen_21fc30db2a015ba1 ("
  , "  E *arg1"
  , ")"
  , "{"
  , "  (fooE)(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_9dbeca9fa307eee9" hs_bindgen_9dbeca9fa307eee9_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_foo@
hs_bindgen_9dbeca9fa307eee9 ::
     BG.Ptr MyStruct
  -> IO ()
hs_bindgen_9dbeca9fa307eee9 =
  BG.fromFFIType hs_bindgen_9dbeca9fa307eee9_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 8:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_9dbeca9fa307eee9 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_fb94ee6f22de1d89" hs_bindgen_fb94ee6f22de1d89_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooA@
hs_bindgen_fb94ee6f22de1d89 ::
     BG.Ptr A
  -> IO ()
hs_bindgen_fb94ee6f22de1d89 =
  BG.fromFFIType hs_bindgen_fb94ee6f22de1d89_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 14:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_fb94ee6f22de1d89 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_f8f83a7090687b7e" hs_bindgen_f8f83a7090687b7e_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooB@
hs_bindgen_f8f83a7090687b7e ::
     BG.Ptr B
  -> IO ()
hs_bindgen_f8f83a7090687b7e =
  BG.fromFFIType hs_bindgen_f8f83a7090687b7e_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 15:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_f8f83a7090687b7e x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooC@
foreign import ccall unsafe "hs_bindgen_f1d253a32d18ddb1" hs_bindgen_f1d253a32d18ddb1_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooC@
hs_bindgen_f1d253a32d18ddb1 ::
     BG.Ptr M.C
  -> IO ()
hs_bindgen_f1d253a32d18ddb1 =
  BG.fromFFIType hs_bindgen_f1d253a32d18ddb1_base

{-| __C declaration:__ @fooC@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 37:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooC ::
     M.C
     -- ^ __C declaration:__ @x@
  -> IO ()
fooC =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_f1d253a32d18ddb1 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooD@
foreign import ccall unsafe "hs_bindgen_2ee0831a468707cf" hs_bindgen_2ee0831a468707cf_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooD@
hs_bindgen_2ee0831a468707cf ::
     BG.Ptr M.D
  -> IO ()
hs_bindgen_2ee0831a468707cf =
  BG.fromFFIType hs_bindgen_2ee0831a468707cf_base

{-| __C declaration:__ @fooD@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 38:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooD ::
     M.D
     -- ^ __C declaration:__ @x@
  -> IO ()
fooD =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_2ee0831a468707cf x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooE@
foreign import ccall unsafe "hs_bindgen_21fc30db2a015ba1" hs_bindgen_21fc30db2a015ba1_base ::
     BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooE@
hs_bindgen_21fc30db2a015ba1 ::
     BG.Ptr E
  -> IO ()
hs_bindgen_21fc30db2a015ba1 =
  BG.fromFFIType hs_bindgen_21fc30db2a015ba1_base

{-| __C declaration:__ @fooE@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 39:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooE ::
     E
     -- ^ __C declaration:__ @x@
  -> IO ()
fooE =
  \x0 ->
    BG.with x0 (\x1 -> hs_bindgen_21fc30db2a015ba1 x1)
