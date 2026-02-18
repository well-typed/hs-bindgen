{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <edge-cases/distilled_lib_1.h>"
  , "/* test_edgecasesdistilled_lib_1_Example_get_v */"
  , "__attribute__ ((const))"
  , "var_t *hs_bindgen_226b16768d8906f3 (void)"
  , "{"
  , "  return &v;"
  , "}"
  ]))

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_v@
foreign import ccall unsafe "hs_bindgen_226b16768d8906f3" hs_bindgen_226b16768d8906f3_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_v@
hs_bindgen_226b16768d8906f3 :: IO (RIP.Ptr Var_t)
hs_bindgen_226b16768d8906f3 =
  RIP.fromFFIType hs_bindgen_226b16768d8906f3_base

{-# NOINLINE v #-}
{-| __C declaration:__ @v@

    __defined at:__ @edge-cases\/distilled_lib_1.h 91:14@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
v :: RIP.Ptr Var_t
v = RIP.unsafePerformIO hs_bindgen_226b16768d8906f3
