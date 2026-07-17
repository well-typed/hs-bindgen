{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe
    ( Example.Safe.multiply_complex_f
    , Example.Safe.add_complex
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "void hs_bindgen_687af703c95fba0e ("
  , "  float _Complex *arg1,"
  , "  float _Complex *arg2,"
  , "  float _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = (multiply_complex_f)(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_3ff14ee8c5914fc6 ("
  , "  double _Complex *arg1,"
  , "  double _Complex *arg2,"
  , "  double _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = (add_complex)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
foreign import ccall safe "hs_bindgen_687af703c95fba0e" hs_bindgen_687af703c95fba0e_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
hs_bindgen_687af703c95fba0e ::
     BG.Ptr (BG.Complex BG.CFloat)
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> BG.Ptr (BG.Complex BG.CFloat)
  -> IO ()
hs_bindgen_687af703c95fba0e =
  BG.fromFFIType hs_bindgen_687af703c95fba0e_base

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h 21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f ::
     BG.Complex BG.CFloat
     -- ^ __C declaration:__ @a@
  -> BG.Complex BG.CFloat
     -- ^ __C declaration:__ @b@
  -> IO (BG.Complex BG.CFloat)
multiply_complex_f =
  \a0 ->
    \b1 ->
      BG.with a0 (\a2 ->
                    BG.with b1 (\b3 ->
                                  BG.allocaAndPeek (\res4 ->
                                                      hs_bindgen_687af703c95fba0e a2 b3 res4)))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
foreign import ccall safe "hs_bindgen_3ff14ee8c5914fc6" hs_bindgen_3ff14ee8c5914fc6_base ::
     BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> BG.Ptr BG.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
hs_bindgen_3ff14ee8c5914fc6 ::
     BG.Ptr (BG.Complex BG.CDouble)
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> BG.Ptr (BG.Complex BG.CDouble)
  -> IO ()
hs_bindgen_3ff14ee8c5914fc6 =
  BG.fromFFIType hs_bindgen_3ff14ee8c5914fc6_base

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h 22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex ::
     BG.Complex BG.CDouble
     -- ^ __C declaration:__ @a@
  -> BG.Complex BG.CDouble
     -- ^ __C declaration:__ @b@
  -> IO (BG.Complex BG.CDouble)
add_complex =
  \a0 ->
    \b1 ->
      BG.with a0 (\a2 ->
                    BG.with b1 (\b3 ->
                                  BG.allocaAndPeek (\res4 ->
                                                      hs_bindgen_3ff14ee8c5914fc6 a2 b3 res4)))
