{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
foreign import ccall safe "hs_bindgen_f2a9c7d0ba1aaa3b" hs_bindgen_f2a9c7d0ba1aaa3b ::
     Ptr.Ptr MyStruct
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_f2a9c7d0ba1aaa3b

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
    F.with x0 (\y1 -> hs_bindgen_f2a9c7d0ba1aaa3b y1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooA@
foreign import ccall safe "hs_bindgen_d7efef1db7e6b005" hs_bindgen_d7efef1db7e6b005 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_d7efef1db7e6b005

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
    F.with x0 (\y1 -> hs_bindgen_d7efef1db7e6b005 y1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Safe_fooB@
foreign import ccall safe "hs_bindgen_e49c2e985e471c99" hs_bindgen_e49c2e985e471c99 ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_e49c2e985e471c99

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
    F.with x0 (\y1 -> hs_bindgen_e49c2e985e471c99 y1)
