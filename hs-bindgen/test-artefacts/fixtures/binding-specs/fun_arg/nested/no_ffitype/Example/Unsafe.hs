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
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <binding-specs/fun_arg/nested/no_ffitype.h>"
  , "void hs_bindgen_c216f054753a1120 ("
  , "  void (*arg1) ("
  , "  C arg1,"
  , "  T arg2"
  , ")"
  , ")"
  , "{"
  , "  (f)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_bindingspecsfun_argnestedn_Example_Unsafe_f@
foreign import ccall unsafe "hs_bindgen_c216f054753a1120" hs_bindgen_c216f054753a1120_base ::
     BG.FunPtr BG.Void
  -> IO ()

-- __unique:__ @test_bindingspecsfun_argnestedn_Example_Unsafe_f@
hs_bindgen_c216f054753a1120 ::
     BG.FunPtr (M.C -> T -> IO ())
  -> IO ()
hs_bindgen_c216f054753a1120 =
  \x0 ->
    hs_bindgen_c216f054753a1120_base (BG.toFFIType x0)

{-| __C declaration:__ @f@

    __defined at:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h 9:6@

    __exported by:__ @binding-specs\/fun_arg\/nested\/no_ffitype.h@
-}
f ::
     BG.FunPtr (M.C -> T -> IO ())
     -- ^ __C declaration:__ @x@
  -> IO ()
f = hs_bindgen_c216f054753a1120
