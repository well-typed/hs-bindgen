{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "void hs_bindgen_e5e3172c2163672b ("
  , "  float _Complex *arg1,"
  , "  float _Complex *arg2,"
  , "  float _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = (multiply_complex_f)(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_28f2705e917973ab ("
  , "  double _Complex *arg1,"
  , "  double _Complex *arg2,"
  , "  double _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = (add_complex)(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_multiply_complex_f@
foreign import ccall unsafe "hs_bindgen_e5e3172c2163672b" hs_bindgen_e5e3172c2163672b_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_multiply_complex_f@
hs_bindgen_e5e3172c2163672b ::
     RIP.Ptr (RIP.Complex RIP.CFloat)
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> RIP.Ptr (RIP.Complex RIP.CFloat)
  -> IO ()
hs_bindgen_e5e3172c2163672b =
  RIP.fromFFIType hs_bindgen_e5e3172c2163672b_base

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
                                                         hs_bindgen_e5e3172c2163672b a2 b3 res4)))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_add_complex@
foreign import ccall unsafe "hs_bindgen_28f2705e917973ab" hs_bindgen_28f2705e917973ab_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_add_complex@
hs_bindgen_28f2705e917973ab ::
     RIP.Ptr (RIP.Complex RIP.CDouble)
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> RIP.Ptr (RIP.Complex RIP.CDouble)
  -> IO ()
hs_bindgen_28f2705e917973ab =
  RIP.fromFFIType hs_bindgen_28f2705e917973ab_base

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
                                                         hs_bindgen_28f2705e917973ab a2 b3 res4)))
