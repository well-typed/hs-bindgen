{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe
    ( Example.Unsafe.use_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "void hs_bindgen_495d2cdae3c804b4 ("
  , "  A arg1"
  , ")"
  , "{"
  , "  (use_b)(arg1);"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_1_Example_Unsafe_use_b@
foreign import ccall unsafe "hs_bindgen_495d2cdae3c804b4" hs_bindgen_495d2cdae3c804b4_base ::
     BG.Int32
  -> IO ()

-- __unique:__ @test_macrosmacro_ext_binding_dep_1_Example_Unsafe_use_b@
hs_bindgen_495d2cdae3c804b4 ::
     M.A
  -> IO ()
hs_bindgen_495d2cdae3c804b4 =
  BG.fromFFIType hs_bindgen_495d2cdae3c804b4_base

{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b ::
     M.A
     -- ^ __C declaration:__ @x@
  -> IO ()
use_b = hs_bindgen_495d2cdae3c804b4
