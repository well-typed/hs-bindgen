{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "/* test_typescomplexhsb_complex_test_Example_get_multiply_complex_f */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_29b4fea741132943 (void)) ("
  , "  float _Complex arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &multiply_complex_f;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_add_complex */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_b53577d6ad8dd36c (void)) ("
  , "  double _Complex arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &add_complex;"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_multiply_complex_f@
foreign import ccall unsafe "hs_bindgen_29b4fea741132943" hs_bindgen_29b4fea741132943_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_multiply_complex_f@
hs_bindgen_29b4fea741132943 :: IO (RIP.FunPtr ((RIP.Complex RIP.CFloat) -> (RIP.Complex RIP.CFloat) -> IO (RIP.Complex RIP.CFloat)))
hs_bindgen_29b4fea741132943 =
  RIP.fromFFIType hs_bindgen_29b4fea741132943_base

{-# NOINLINE multiply_complex_f #-}
{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h 21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f :: RIP.FunPtr ((RIP.Complex RIP.CFloat) -> (RIP.Complex RIP.CFloat) -> IO (RIP.Complex RIP.CFloat))
multiply_complex_f =
  RIP.unsafePerformIO hs_bindgen_29b4fea741132943

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_add_complex@
foreign import ccall unsafe "hs_bindgen_b53577d6ad8dd36c" hs_bindgen_b53577d6ad8dd36c_base ::
     IO (RIP.FunPtr RIP.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_add_complex@
hs_bindgen_b53577d6ad8dd36c :: IO (RIP.FunPtr ((RIP.Complex RIP.CDouble) -> (RIP.Complex RIP.CDouble) -> IO (RIP.Complex RIP.CDouble)))
hs_bindgen_b53577d6ad8dd36c =
  RIP.fromFFIType hs_bindgen_b53577d6ad8dd36c_base

{-# NOINLINE add_complex #-}
{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h 22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex :: RIP.FunPtr ((RIP.Complex RIP.CDouble) -> (RIP.Complex RIP.CDouble) -> IO (RIP.Complex RIP.CDouble))
add_complex =
  RIP.unsafePerformIO hs_bindgen_b53577d6ad8dd36c
