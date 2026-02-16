{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/vector_test.h>"
  , "/* test_typescomplexvector_test_Example_get_new_vector */"
  , "__attribute__ ((const))"
  , "vector *(*hs_bindgen_cb36cf0957839e33 (void)) ("
  , "  double arg1,"
  , "  double arg2"
  , ")"
  , "{"
  , "  return &new_vector;"
  , "}"
  ]))

-- __unique:__ @test_typescomplexvector_test_Example_get_new_vector@
foreign import ccall unsafe "hs_bindgen_cb36cf0957839e33" hs_bindgen_cb36cf0957839e33_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typescomplexvector_test_Example_get_new_vector@
hs_bindgen_cb36cf0957839e33 :: IO (RIP.FunPtr (RIP.CDouble -> RIP.CDouble -> IO (RIP.Ptr Vector)))
hs_bindgen_cb36cf0957839e33 =
  RIP.fromFFIType hs_bindgen_cb36cf0957839e33_base

{-# NOINLINE new_vector #-}
{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h 6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector :: RIP.FunPtr (RIP.CDouble -> RIP.CDouble -> IO (RIP.Ptr Vector))
new_vector =
  RIP.unsafePerformIO hs_bindgen_cb36cf0957839e33
