{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "void hs_bindgen_687af703c95fba0e ("
  , "  float _Complex *arg1,"
  , "  float _Complex *arg2,"
  , "  float _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = multiply_complex_f(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_3ff14ee8c5914fc6 ("
  , "  double _Complex *arg1,"
  , "  double _Complex *arg2,"
  , "  double _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = add_complex(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
foreign import ccall safe "hs_bindgen_687af703c95fba0e" hs_bindgen_687af703c95fba0e_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
hs_bindgen_687af703c95fba0e ::
     RIP.Ptr (RIP.Complex RIP.CFloat)
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_687af703c95fba0e =
  RIP.fromFFIType hs_bindgen_687af703c95fba0e_base

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h 21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f ::
     RIP.Complex RIP.CFloat
     -- ^ __C declaration:__ @a@
  -> RIP.Complex RIP.CFloat
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Complex RIP.CFloat)
multiply_complex_f =
  \a0 ->
    \b1 ->
      RIP.with a0 (\a2 ->
                     RIP.with b1 (\b3 ->
                                    RIP.allocaAndPeek (\res4 ->
                                                         hs_bindgen_687af703c95fba0e a2 b3 res4)))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
foreign import ccall safe "hs_bindgen_3ff14ee8c5914fc6" hs_bindgen_3ff14ee8c5914fc6_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
hs_bindgen_3ff14ee8c5914fc6 ::
     RIP.Ptr (RIP.Complex RIP.CDouble)
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_3ff14ee8c5914fc6 =
  RIP.fromFFIType hs_bindgen_3ff14ee8c5914fc6_base

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h 22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex ::
     RIP.Complex RIP.CDouble
     -- ^ __C declaration:__ @a@
  -> RIP.Complex RIP.CDouble
     -- ^ __C declaration:__ @b@
  -> IO (RIP.Complex RIP.CDouble)
add_complex =
  \a0 ->
    \b1 ->
      RIP.with a0 (\a2 ->
                     RIP.with b1 (\b3 ->
                                    RIP.allocaAndPeek (\res4 ->
                                                         hs_bindgen_3ff14ee8c5914fc6 a2 b3 res4)))
