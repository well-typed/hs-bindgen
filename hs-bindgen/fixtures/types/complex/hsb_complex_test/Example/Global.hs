{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Data.Complex
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "/* Example_get_global_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_ecedfd9f3efcf9a8 (void)"
  , "{"
  , "  return &global_complex_float;"
  , "}"
  , "/* Example_get_global_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_bbff9f51358a2ee7 (void)"
  , "{"
  , "  return &global_complex_double;"
  , "}"
  , "/* Example_get_global_complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_be2baa8e46d89330 (void)"
  , "{"
  , "  return &global_complex_float_flipped;"
  , "}"
  , "/* Example_get_global_complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_18101f0ad07c7118 (void)"
  , "{"
  , "  return &global_complex_double_flipped;"
  , "}"
  , "/* Example_get_global_Complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_453812c7adb09751 (void)"
  , "{"
  , "  return &global_Complex_float;"
  , "}"
  , "/* Example_get_global_Complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_49d7bb1ada9d9366 (void)"
  , "{"
  , "  return &global_Complex_double;"
  , "}"
  , "/* Example_get_global_Complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_fbaae6d3c15d0f63 (void)"
  , "{"
  , "  return &global_Complex_float_flipped;"
  , "}"
  , "/* Example_get_global_Complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_c332651c627dc427 (void)"
  , "{"
  , "  return &global_Complex_double_flipped;"
  , "}"
  , "/* Example_get_const_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex const *hs_bindgen_test_typescomplexhsb_complex_test_4d059fc79bb89894 (void)"
  , "{"
  , "  return &const_complex_float;"
  , "}"
  , "/* Example_get_const_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex const *hs_bindgen_test_typescomplexhsb_complex_test_95a4edcc0f996f70 (void)"
  , "{"
  , "  return &const_complex_double;"
  , "}"
  , "/* Example_get_volatile_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_329349d35aef5e10 (void)"
  , "{"
  , "  return &volatile_complex_float;"
  , "}"
  , "/* Example_get_volatile_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_7c3ccbd8bf1c7730 (void)"
  , "{"
  , "  return &volatile_complex_double;"
  , "}"
  , "/* Example_get_complex_float_array_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_typescomplexhsb_complex_test_7a3792988944f3d2 (void))[10]"
  , "{"
  , "  return &complex_float_array;"
  , "}"
  , "/* Example_get_complex_double_array_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_typescomplexhsb_complex_test_8b8ce8a9f096c34e (void))[10]"
  , "{"
  , "  return &complex_double_array;"
  , "}"
  ]))

{-| __unique:__ @Example_get_global_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_ecedfd9f3efcf9a8" hs_bindgen_test_typescomplexhsb_complex_test_ecedfd9f3efcf9a8 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_ptr #-}

{-| __C declaration:__ @global_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:3:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_ecedfd9f3efcf9a8

{-| __unique:__ @Example_get_global_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_bbff9f51358a2ee7" hs_bindgen_test_typescomplexhsb_complex_test_bbff9f51358a2ee7 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_ptr #-}

{-| __C declaration:__ @global_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:4:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_bbff9f51358a2ee7

{-| __unique:__ @Example_get_global_complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_be2baa8e46d89330" hs_bindgen_test_typescomplexhsb_complex_test_be2baa8e46d89330 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:6:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_be2baa8e46d89330

{-| __unique:__ @Example_get_global_complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_18101f0ad07c7118" hs_bindgen_test_typescomplexhsb_complex_test_18101f0ad07c7118 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:7:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_18101f0ad07c7118

{-| __unique:__ @Example_get_global_Complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_453812c7adb09751" hs_bindgen_test_typescomplexhsb_complex_test_453812c7adb09751 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_ptr #-}

{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:9:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_453812c7adb09751

{-| __unique:__ @Example_get_global_Complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_49d7bb1ada9d9366" hs_bindgen_test_typescomplexhsb_complex_test_49d7bb1ada9d9366 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_ptr #-}

{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:10:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_49d7bb1ada9d9366

{-| __unique:__ @Example_get_global_Complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_fbaae6d3c15d0f63" hs_bindgen_test_typescomplexhsb_complex_test_fbaae6d3c15d0f63 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:12:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_fbaae6d3c15d0f63

{-| __unique:__ @Example_get_global_Complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_c332651c627dc427" hs_bindgen_test_typescomplexhsb_complex_test_c332651c627dc427 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:13:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_c332651c627dc427

{-| __unique:__ @Example_get_const_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_4d059fc79bb89894" hs_bindgen_test_typescomplexhsb_complex_test_4d059fc79bb89894 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE const_complex_float_ptr #-}

{-| __C declaration:__ @const_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:15:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
const_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_4d059fc79bb89894

{-# NOINLINE const_complex_float #-}

const_complex_float :: Data.Complex.Complex FC.CFloat
const_complex_float =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_float_ptr)

{-| __unique:__ @Example_get_const_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_95a4edcc0f996f70" hs_bindgen_test_typescomplexhsb_complex_test_95a4edcc0f996f70 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE const_complex_double_ptr #-}

{-| __C declaration:__ @const_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:16:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
const_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_95a4edcc0f996f70

{-# NOINLINE const_complex_double #-}

const_complex_double :: Data.Complex.Complex FC.CDouble
const_complex_double =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_double_ptr)

{-| __unique:__ @Example_get_volatile_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_329349d35aef5e10" hs_bindgen_test_typescomplexhsb_complex_test_329349d35aef5e10 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE volatile_complex_float_ptr #-}

{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:18:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
volatile_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_329349d35aef5e10

{-| __unique:__ @Example_get_volatile_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_7c3ccbd8bf1c7730" hs_bindgen_test_typescomplexhsb_complex_test_7c3ccbd8bf1c7730 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE volatile_complex_double_ptr #-}

{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:19:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
volatile_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_7c3ccbd8bf1c7730

{-| __unique:__ @Example_get_complex_float_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_7a3792988944f3d2" hs_bindgen_test_typescomplexhsb_complex_test_7a3792988944f3d2 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE complex_float_array_ptr #-}

{-| __C declaration:__ @complex_float_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:30:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_float_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat))
complex_float_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_7a3792988944f3d2

{-| __unique:__ @Example_get_complex_double_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_8b8ce8a9f096c34e" hs_bindgen_test_typescomplexhsb_complex_test_8b8ce8a9f096c34e ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE complex_double_array_ptr #-}

{-| __C declaration:__ @complex_double_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:31:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_double_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble))
complex_double_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_8b8ce8a9f096c34e
