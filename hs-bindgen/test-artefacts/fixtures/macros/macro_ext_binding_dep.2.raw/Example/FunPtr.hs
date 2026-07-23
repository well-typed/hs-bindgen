{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.use_b
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import qualified M

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "/* test_macrosmacro_ext_binding_dep_2_Example_get_use_b */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2ea258d8b3c67286 (void)) ("
  , "  A arg1"
  , ")"
  , "{"
  , "  return &use_b;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_2_Example_get_use_b@
foreign import ccall unsafe "hs_bindgen_2ea258d8b3c67286" hs_bindgen_2ea258d8b3c67286_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_macrosmacro_ext_binding_dep_2_Example_get_use_b@
hs_bindgen_2ea258d8b3c67286 :: IO (BG.FunPtr (M.A -> IO ()))
hs_bindgen_2ea258d8b3c67286 =
  BG.fromFFIType hs_bindgen_2ea258d8b3c67286_base

{-# NOINLINE use_b #-}
{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b :: BG.FunPtr (M.A -> IO ())
use_b =
  BG.unsafePerformIO hs_bindgen_2ea258d8b3c67286
