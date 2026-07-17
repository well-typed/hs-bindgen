{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.v
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_v@
hs_bindgen_226b16768d8906f3 :: IO (BG.Ptr Var_t)
hs_bindgen_226b16768d8906f3 =
  BG.fromFFIType hs_bindgen_226b16768d8906f3_base

{-# NOINLINE v #-}
{-| __C declaration:__ @v@

    __defined at:__ @edge-cases\/distilled_lib_1.h 91:14@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
v :: BG.Ptr Var_t
v = BG.unsafePerformIO hs_bindgen_226b16768d8906f3
