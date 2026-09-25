{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.use_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "void hs_bindgen_f8c6775667265898 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (use_b)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_1_Example_Safe_use_b@
foreign import ccall safe "hs_bindgen_f8c6775667265898" hs_bindgen_f8c6775667265898_base ::
     BG.CInt
  -> IO ()

-- __unique:__ @test_macrosmacro_ext_binding_dep_1_Example_Safe_use_b@
hs_bindgen_f8c6775667265898 ::
     M.A
  -> IO ()
hs_bindgen_f8c6775667265898 =
  \x0 ->
    hs_bindgen_f8c6775667265898_base (BG.toFFIType x0)

{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b ::
     M.A
     -- ^ __C declaration:__ @x@
  -> IO ()
use_b = hs_bindgen_f8c6775667265898
