{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/vector_test.h>"
  , "vector *hs_bindgen_cd5f566bc96dcba0 ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return (new_vector)(arg1, arg2);"
  , "}"
  ]))

-- __unique:__ @test_typescomplexvector_test_Example_Safe_new_vector@
foreign import ccall safe "hs_bindgen_cd5f566bc96dcba0" hs_bindgen_cd5f566bc96dcba0_base ::
     Double
  -> Double
  -> IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_typescomplexvector_test_Example_Safe_new_vector@
hs_bindgen_cd5f566bc96dcba0 ::
     RIP.CDouble
  -> RIP.CDouble
  -> IO (RIP.Ptr Vector)
hs_bindgen_cd5f566bc96dcba0 =
  RIP.fromFFIType hs_bindgen_cd5f566bc96dcba0_base

{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h 6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector ::
     RIP.CDouble
     -- ^ __C declaration:__ @x@
  -> RIP.CDouble
     -- ^ __C declaration:__ @y@
  -> IO (RIP.Ptr Vector)
new_vector = hs_bindgen_cd5f566bc96dcba0
