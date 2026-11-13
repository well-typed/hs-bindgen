{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.f
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/nested/struct_ffitype.h>"
  , "void hs_bindgen_00ed18f111fbbdfb ("
  , "  void (*arg1) ("
  , "  C arg1"
  , ")"
  , ")"
  , "{"
  , "  (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argnesteds_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_00ed18f111fbbdfb" hs_bindgen_00ed18f111fbbdfb_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argnesteds_Example_Unsafe_f@
hs_bindgen_00ed18f111fbbdfb ::
     BG.FunPtr (M.C -> IO ())
  -> IO ()
hs_bindgen_00ed18f111fbbdfb =
  \x0 ->
    hs_bindgen_00ed18f111fbbdfb_base (BG.toFFIType x0)

{-| __C declaration:__ @f@

    __defined at:__ @binding-specs\/fun_arg\/nested\/struct_ffitype.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/nested\/struct_ffitype.h@
-}
f ::
     BG.FunPtr (M.C -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_00ed18f111fbbdfb
