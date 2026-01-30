{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Example
import Prelude (IO)

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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_edgecasesdistilled_lib_1_Example_get_v@
hs_bindgen_226b16768d8906f3 :: IO (Ptr.Ptr Var_t)
hs_bindgen_226b16768d8906f3 =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_226b16768d8906f3_base

{-# NOINLINE v #-}
{-| __C declaration:__ @v@

    __defined at:__ @edge-cases\/distilled_lib_1.h 91:14@

    __exported by:__ @edge-cases\/distilled_lib_1.h@
-}
v :: Ptr.Ptr Var_t
v =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_226b16768d8906f3
