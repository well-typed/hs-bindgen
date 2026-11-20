{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.selected
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/deselected.h>"
  , "void hs_bindgen_4a55dd0330da833d ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  (selected)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesde_Example_Unsafe_selected@
foreign import ccall unsafe "hs_bindgen_4a55dd0330da833d" hs_bindgen_4a55dd0330da833d_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_functionsfun_ptr_instancesde_Example_Unsafe_selected@
hs_bindgen_4a55dd0330da833d ::
     BG.FunPtr (T -> IO ())
  -> IO ()
hs_bindgen_4a55dd0330da833d =
  \x0 ->
    hs_bindgen_4a55dd0330da833d_base (BG.toFFIType x0)

{-| __C declaration:__ @selected@

    __defined at:__ @functions\/fun_ptr_instances\/deselected.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/deselected.h@
-}
selected ::
     BG.FunPtr (T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
selected = hs_bindgen_4a55dd0330da833d
