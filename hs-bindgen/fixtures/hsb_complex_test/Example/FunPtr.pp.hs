{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

import qualified Data.Complex
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <hsb_complex_test.h>"
  , "/* get_multiply_complex_f_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56 (void)) ("
  , "  float _Complex arg1,"
  , "  float _Complex arg2"
  , ")"
  , "{"
  , "  return &multiply_complex_f;"
  , "}"
  , "/* get_add_complex_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f (void)) ("
  , "  double _Complex arg1,"
  , "  double _Complex arg2"
  , ")"
  , "{"
  , "  return &add_complex;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56" hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56 ::
     IO (Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE multiply_complex_f_ptr #-}

{-| __C declaration:__ @multiply_complex_f@

    __defined at:__ @hsb_complex_test.h:21:16@

    __exported by:__ @hsb_complex_test.h@
-}
multiply_complex_f_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CFloat) -> (Data.Complex.Complex FC.CFloat) -> IO (Data.Complex.Complex FC.CFloat))
multiply_complex_f_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_hsb_complex_test_a7d89c01385c8c56

foreign import ccall unsafe "hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f" hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f ::
     IO (Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE add_complex_ptr #-}

{-| __C declaration:__ @add_complex@

    __defined at:__ @hsb_complex_test.h:22:16@

    __exported by:__ @hsb_complex_test.h@
-}
add_complex_ptr :: Ptr.FunPtr ((Data.Complex.Complex FC.CDouble) -> (Data.Complex.Complex FC.CDouble) -> IO (Data.Complex.Complex FC.CDouble))
add_complex_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_hsb_complex_test_b6226a5bde741b3f
