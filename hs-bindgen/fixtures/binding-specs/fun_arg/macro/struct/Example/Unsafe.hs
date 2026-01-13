{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign as F
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
foreign import ccall unsafe "hs_bindgen_9dbeca9fa307eee9" hs_bindgen_9dbeca9fa307eee9 ::
     Ptr.Ptr MyStruct
  -> IO ()

{-| Pointer-based API for 'foo'
-}
foo_wrapper ::
     Ptr.Ptr MyStruct
     -- ^ __C declaration:__ @x@
  -> IO ()
foo_wrapper = hs_bindgen_9dbeca9fa307eee9

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
    F.with x0 (\y1 -> hs_bindgen_9dbeca9fa307eee9 y1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooA@
foreign import ccall unsafe "hs_bindgen_fb94ee6f22de1d89" hs_bindgen_fb94ee6f22de1d89 ::
     Ptr.Ptr A
  -> IO ()

{-| Pointer-based API for 'fooA'
-}
fooA_wrapper ::
     Ptr.Ptr A
     -- ^ __C declaration:__ @x@
  -> IO ()
fooA_wrapper = hs_bindgen_fb94ee6f22de1d89

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
    F.with x0 (\y1 -> hs_bindgen_fb94ee6f22de1d89 y1)

-- __unique:__ @test_bindingspecsfun_argmacrost_Example_Unsafe_fooB@
foreign import ccall unsafe "hs_bindgen_f8f83a7090687b7e" hs_bindgen_f8f83a7090687b7e ::
     Ptr.Ptr B
  -> IO ()

{-| Pointer-based API for 'fooB'
-}
fooB_wrapper ::
     Ptr.Ptr B
     -- ^ __C declaration:__ @x@
  -> IO ()
fooB_wrapper = hs_bindgen_f8f83a7090687b7e

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
    F.with x0 (\y1 -> hs_bindgen_f8f83a7090687b7e y1)
