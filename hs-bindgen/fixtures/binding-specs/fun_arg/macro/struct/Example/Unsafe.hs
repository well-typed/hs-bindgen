{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "void hs_bindgen_9dbeca9fa307eee9 ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_fb94ee6f22de1d89 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_f8f83a7090687b7e ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_foo@
foreign import ccall unsafe "hs_bindgen_9dbeca9fa307eee9" hs_bindgen_9dbeca9fa307eee9_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_foo@
hs_bindgen_9dbeca9fa307eee9 ::
     RIP.Ptr MyStruct
  -> IO ()
hs_bindgen_9dbeca9fa307eee9 =
  RIP.fromFFIType hs_bindgen_9dbeca9fa307eee9_base

{-| __C declaration:__ @foo@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 5:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
foo ::
     MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_9dbeca9fa307eee9 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_fb94ee6f22de1d89" hs_bindgen_fb94ee6f22de1d89_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooA@
hs_bindgen_fb94ee6f22de1d89 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_fb94ee6f22de1d89 =
  RIP.fromFFIType hs_bindgen_fb94ee6f22de1d89_base

{-| __C declaration:__ @fooA@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 10:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooA ::
     A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_fb94ee6f22de1d89 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_f8f83a7090687b7e" hs_bindgen_f8f83a7090687b7e_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooB@
hs_bindgen_f8f83a7090687b7e ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_f8f83a7090687b7e =
  RIP.fromFFIType hs_bindgen_f8f83a7090687b7e_base

{-| __C declaration:__ @fooB@

    __defined at:__ @binding-specs\/fun_arg\/macro\/struct.h 11:6@

    __exported by:__ @binding-specs\/fun_arg\/macro\/struct.h@
-}
fooB ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB =
  \x0 ->
    RIP.with x0 (\x1 -> hs_bindgen_f8f83a7090687b7e x1)
