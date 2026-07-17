{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.new_vector
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
  -> IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexvector_test_Example_Safe_new_vector@
hs_bindgen_cd5f566bc96dcba0 ::
     BG.CDouble
  -> BG.CDouble
  -> IO (BG.Ptr Vector)
hs_bindgen_cd5f566bc96dcba0 =
  BG.fromFFIType hs_bindgen_cd5f566bc96dcba0_base

{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h 6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector ::
     BG.CDouble
     -- ^ __C declaration:__ @x@
  -> BG.CDouble
     -- ^ __C declaration:__ @y@
  -> IO (BG.Ptr Vector)
new_vector = hs_bindgen_cd5f566bc96dcba0
