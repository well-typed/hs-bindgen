{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.selected
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example
import Prelude (IO, fmap)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <functions/fun_ptr_instances/deselected.h>"
  , "/* test_functionsfun_ptr_instancesde_Example_get_selected */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_8401672be735e3ae (void)) ("
  , "  void (*arg1) ("
  , "  T arg1"
  , ")"
  , ")"
  , "{"
  , "  return &selected;"
  , "}"
  ]))

-- __unique:__ @test_functionsfun_ptr_instancesde_Example_get_selected@
foreign import ccall unsafe "hs_bindgen_8401672be735e3ae" hs_bindgen_8401672be735e3ae_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_functionsfun_ptr_instancesde_Example_get_selected@
hs_bindgen_8401672be735e3ae :: IO (BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ()))
hs_bindgen_8401672be735e3ae =
  fmap BG.fromFFIType hs_bindgen_8401672be735e3ae_base

{-# NOINLINE selected #-}
{-| __C declaration:__ @selected@

    __defined at:__ @functions\/fun_ptr_instances\/deselected.h 7:6@

    __exported by:__ @functions\/fun_ptr_instances\/deselected.h@
-}
selected :: BG.FunPtr (BG.FunPtr (T -> IO ()) -> IO ())
selected =
  BG.unsafePerformIO hs_bindgen_8401672be735e3ae
