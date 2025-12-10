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
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

{-| Pointer-based API for 'multiply_complex_f'

__unique:__ @test_typescomplexhsb_complex_test_Example_Safe_multiply_complex_f@
-}
foreign import ccall safe "hs_bindgen_687af703c95fba0e" multiply_complex_f_wrapper ::
     Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> Ptr.Ptr (Data.Complex.Complex FC.CFloat)
  -> IO ()

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h:21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f ::
     Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @a@
  -> Data.Complex.Complex FC.CFloat
     -- ^ __C declaration:__ @b@
  -> IO (Data.Complex.Complex FC.CFloat)
multiply_complex_f =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        multiply_complex_f_wrapper y3 y2 z4)))

{-| Pointer-based API for 'add_complex'

__unique:__ @test_typescomplexhsb_complex_test_Example_Safe_add_complex@
-}
foreign import ccall safe "hs_bindgen_3ff14ee8c5914fc6" add_complex_wrapper ::
     Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> Ptr.Ptr (Data.Complex.Complex FC.CDouble)
  -> IO ()

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h:22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex ::
     Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @a@
  -> Data.Complex.Complex FC.CDouble
     -- ^ __C declaration:__ @b@
  -> IO (Data.Complex.Complex FC.CDouble)
add_complex =
  \x0 ->
    \x1 ->
      F.with x1 (\y2 ->
                   F.with x0 (\y3 ->
                                HsBindgen.Runtime.CAPI.allocaAndPeek (\z4 ->
                                                                        add_complex_wrapper y3 y2 z4)))
