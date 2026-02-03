{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "void hs_bindgen_e5e3172c2163672b ("
  , "  float _Complex *arg1,"
  , "  float _Complex *arg2,"
  , "  float _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = multiply_complex_f(*arg1, *arg2);"
  , "}"
  , "void hs_bindgen_28f2705e917973ab ("
  , "  double _Complex *arg1,"
  , "  double _Complex *arg2,"
  , "  double _Complex *arg3"
  , ")"
  , "{"
  , "  *arg3 = add_complex(*arg1, *arg2);"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_multiply_complex_f@
foreign import ccall unsafe "hs_bindgen_e5e3172c2163672b" hs_bindgen_e5e3172c2163672b_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_multiply_complex_f@
hs_bindgen_e5e3172c2163672b ::
     Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()
hs_bindgen_e5e3172c2163672b =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_e5e3172c2163672b_base

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
                                HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res4 ->
                                                                                 hs_bindgen_e5e3172c2163672b a2 b3 res4)))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_add_complex@
foreign import ccall unsafe "hs_bindgen_28f2705e917973ab" hs_bindgen_28f2705e917973ab_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO ()

-- __unique:__ @test_typescomplexhsb_complex_test_Example_Unsafe_add_complex@
hs_bindgen_28f2705e917973ab ::
     Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()
hs_bindgen_28f2705e917973ab =
  HsBindgen.Runtime.Internal.HasFFIType.fromFFIType hs_bindgen_28f2705e917973ab_base

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
                                HsBindgen.Runtime.Internal.CAPI.allocaAndPeek (\res4 ->
                                                                                 hs_bindgen_28f2705e917973ab a2 b3 res4)))
