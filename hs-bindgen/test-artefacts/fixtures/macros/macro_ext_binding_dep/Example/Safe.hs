{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.use_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "void hs_bindgen_f70130e6504e0e5e ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (use_b)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_Safe_use_b@
foreign import ccall safe "hs_bindgen_f70130e6504e0e5e" hs_bindgen_f70130e6504e0e5e_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_Safe_use_b@
hs_bindgen_f70130e6504e0e5e ::
     B
  -> IO ()
hs_bindgen_f70130e6504e0e5e =
  BG.fromFFIType hs_bindgen_f70130e6504e0e5e_base

{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
use_b = hs_bindgen_f70130e6504e0e5e
