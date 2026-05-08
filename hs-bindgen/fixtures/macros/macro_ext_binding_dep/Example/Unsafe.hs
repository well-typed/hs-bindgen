{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.use_b
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "void hs_bindgen_432a5c7b5df51cfa ("
  , "  B arg1"
  , ")"
  , "{"
  , "  (use_b)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_Unsafe_use_b@
foreign import ccall unsafe "hs_bindgen_432a5c7b5df51cfa" hs_bindgen_432a5c7b5df51cfa_base ::
     RIP.Int32
  -> IO ()

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_Unsafe_use_b@
hs_bindgen_432a5c7b5df51cfa ::
     B
  -> IO ()
hs_bindgen_432a5c7b5df51cfa =
  RIP.fromFFIType hs_bindgen_432a5c7b5df51cfa_base

{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b ::
     B
     -- ^ __C declaration:__ @x@
  -> IO ()
use_b = hs_bindgen_432a5c7b5df51cfa
