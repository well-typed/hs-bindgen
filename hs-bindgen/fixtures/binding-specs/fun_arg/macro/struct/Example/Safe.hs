{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <binding-specs/fun_arg/macro/struct.h>"
  , "void hs_bindgen_f2a9c7d0ba1aaa3b ("
  , "  struct MyStruct *arg1"
  , ")"
  , "{"
  , "  foo(*arg1);"
  , "}"
  , "void hs_bindgen_d7efef1db7e6b005 ("
  , "  A *arg1"
  , ")"
  , "{"
  , "  fooA(*arg1);"
  , "}"
  , "void hs_bindgen_e49c2e985e471c99 ("
  , "  B *arg1"
  , ")"
  , "{"
  , "  fooB(*arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
foreign import ccall safe "hs_bindgen_f2a9c7d0ba1aaa3b" hs_bindgen_f2a9c7d0ba1aaa3b_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_foo@
hs_bindgen_f2a9c7d0ba1aaa3b ::
     RIP.Ptr MyStruct
  -> IO ()
hs_bindgen_f2a9c7d0ba1aaa3b =
  RIP.fromFFIType hs_bindgen_f2a9c7d0ba1aaa3b_base

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
    RIP.with x0 (\x1 -> hs_bindgen_f2a9c7d0ba1aaa3b x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_d7efef1db7e6b005" hs_bindgen_d7efef1db7e6b005_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
hs_bindgen_d7efef1db7e6b005 ::
     RIP.Ptr A
  -> IO ()
hs_bindgen_d7efef1db7e6b005 =
  RIP.fromFFIType hs_bindgen_d7efef1db7e6b005_base

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
    RIP.with x0 (\x1 -> hs_bindgen_d7efef1db7e6b005 x1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_e49c2e985e471c99" hs_bindgen_e49c2e985e471c99_base ::
     RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
hs_bindgen_e49c2e985e471c99 ::
     RIP.Ptr B
  -> IO ()
hs_bindgen_e49c2e985e471c99 =
  RIP.fromFFIType hs_bindgen_e49c2e985e471c99_base

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
    RIP.with x0 (\x1 -> hs_bindgen_e49c2e985e471c99 x1)
