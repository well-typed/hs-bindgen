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
  , "/* test_typescomplexhsb_complex_test_Example_get_multiply_complex_f_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_8c8d85daac0162fd (void)) ("
  , "  float _Complex arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &multiply_complex_f;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_add_complex_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_656a87248425c79a (void)) ("
  , "  double _Complex arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &add_complex;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8c8d85daac0162fd" hs_bindgen_8c8d85daac0162fd_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typescomplexhsb_complex_test_Example_get_multiply_complex_f_ptr@
hs_bindgen_8c8d85daac0162fd ::
     IO (Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat)))
hs_bindgen_8c8d85daac0162fd =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8c8d85daac0162fd_base

{-# NOINLINE multiply_complex_f_ptr #-}

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @types\/complex\/hsb_complex_test.h:21:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
multiply_complex_f_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat))
multiply_complex_f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8c8d85daac0162fd

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_656a87248425c79a" hs_bindgen_656a87248425c79a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_typescomplexhsb_complex_test_Example_get_add_complex_ptr@
hs_bindgen_656a87248425c79a ::
     IO (Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble)))
hs_bindgen_656a87248425c79a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_656a87248425c79a_base

{-# NOINLINE add_complex_ptr #-}

{-| __C declaration:__ @add_complex@

    __defined at:__ @types\/complex\/hsb_complex_test.h:22:16@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
add_complex_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble))
add_complex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_656a87248425c79a
