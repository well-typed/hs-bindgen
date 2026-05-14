{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.use_b
    )
  where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <macros/macro_ext_binding_dep.h>"
  , "/* test_macrosmacro_ext_binding_dep_Example_get_use_b */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b341ec07f48c8cc1 (void)) ("
  , "  B arg1"
  , ")"
  , "{"
  , "  return &use_b;"
  , "}"
  ]))

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_get_use_b@
foreign import ccall unsafe "hs_bindgen_b341ec07f48c8cc1" hs_bindgen_b341ec07f48c8cc1_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_macrosmacro_ext_binding_dep_Example_get_use_b@
hs_bindgen_b341ec07f48c8cc1 :: IO (RIP.FunPtr (B -> IO ()))
hs_bindgen_b341ec07f48c8cc1 =
  RIP.fromFFIType hs_bindgen_b341ec07f48c8cc1_base

{-# NOINLINE use_b #-}
{-| __C declaration:__ @use_b@

    __defined at:__ @macros\/macro_ext_binding_dep.h 8:6@

    __exported by:__ @macros\/macro_ext_binding_dep.h@
-}
use_b :: RIP.FunPtr (B -> IO ())
use_b =
  RIP.unsafePerformIO hs_bindgen_b341ec07f48c8cc1
