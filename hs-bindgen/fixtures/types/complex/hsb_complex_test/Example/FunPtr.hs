{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_multiply_complex_f@
hs_bindgen_29b4fea741132943 :: IO (Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat)))
hs_bindgen_29b4fea741132943 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_29b4fea741132943_base

{-# NOINLINE multiply_complex_f #-}
{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h 21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f :: Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat))
multiply_complex_f =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_29b4fea741132943

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_add_complex@
foreign import ccall unsafe "hs_bindgen_b53577d6ad8dd36c" hs_bindgen_b53577d6ad8dd36c_base ::
     IO (Ptr.FunPtr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_add_complex@
hs_bindgen_b53577d6ad8dd36c :: IO (Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble)))
hs_bindgen_b53577d6ad8dd36c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_b53577d6ad8dd36c_base

{-# NOINLINE add_complex #-}
{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h 22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex :: Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble))
add_complex =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b53577d6ad8dd36c
