{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
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
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
hs_bindgen_687af703c95fba0e ::
     Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
hs_bindgen_687af703c95fba0e =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_687af703c95fba0e_base

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h 21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f ::
     Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @a@
  -> Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @b@
  -> IO (Data.Complex.Complex FC.CFloat)
multiply_complex_f =
  \a0 ->
    \b1 ->
      F.with a0 (\a2 ->
                   F.with b1 (\b3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\res4 ->
                                                                        hs_bindgen_687af703c95fba0e a2 b3 res4)))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
foreign import ccall safe "hs_bindgen_3ff14ee8c5914fc6" hs_bindgen_3ff14ee8c5914fc6_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
hs_bindgen_3ff14ee8c5914fc6 ::
     Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
hs_bindgen_3ff14ee8c5914fc6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3ff14ee8c5914fc6_base

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h 22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex ::
     Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @a@
  -> Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @b@
  -> IO (Data.Complex.Complex FC.CDouble)
add_complex =
  \a0 ->
    \b1 ->
      F.with a0 (\a2 ->
                   F.with b1 (\b3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\res4 ->
                                                                        hs_bindgen_3ff14ee8c5914fc6 a2 b3 res4)))
