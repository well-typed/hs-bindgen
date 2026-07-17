{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.new_vector
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_typescomplexvector_test_Example_get_new_vector@
hs_bindgen_cb36cf0957839e33 :: IO (BG.FunPtr (BG.CDouble -> BG.CDouble -> IO (BG.Ptr Vector)))
hs_bindgen_cb36cf0957839e33 =
  BG.fromFFIType hs_bindgen_cb36cf0957839e33_base

{-# NOINLINE new_vector #-}
{-| __C declaration:__ @new_vector@

    __defined at:__ @types\/complex\/vector_test.h 6:9@

    __exported by:__ @types\/complex\/vector_test.h@
-}
new_vector :: BG.FunPtr (BG.CDouble -> BG.CDouble -> IO (BG.Ptr Vector))
new_vector =
  BG.unsafePerformIO hs_bindgen_cb36cf0957839e33
