{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.h
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/struct_result.h>"
  , "void hs_bindgen_6c4023261ffcbb0d ("
  , "  T (*arg1) (void)"
  , ")"
  , "{"
  , "  (h)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesst_Example_Unsafe_h@
foreign import ccall unsafe "hs_bindgen_6c4023261ffcbb0d" hs_bindgen_6c4023261ffcbb0d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesst_Example_Unsafe_h@
hs_bindgen_6c4023261ffcbb0d ::
     BG.FunPtr (IO T)
  -> IO ()
hs_bindgen_6c4023261ffcbb0d =
  \x0 ->
    hs_bindgen_6c4023261ffcbb0d_base (BG.toFFIType x0)

{-| __C declaration:__ @h@

    __defined at:__ @functions\/fun_ptr_instances\/struct_result.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/struct_result.h@
-}
h ::
     BG.FunPtr (IO T)
     -- ^ __C declaration:__ @x@
  -> IO ()
h = hs_bindgen_6c4023261ffcbb0d
