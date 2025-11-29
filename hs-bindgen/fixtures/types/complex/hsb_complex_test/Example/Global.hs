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
  , "/* ExampleNothingget_global_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_69e4d4972011967b (void)"
  , "{"
  , "  return &global_complex_float;"
  , "}"
  , "/* ExampleNothingget_global_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_c3633906ced5dab3 (void)"
  , "{"
  , "  return &global_complex_double;"
  , "}"
  , "/* ExampleNothingget_global_complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_7ef41813e25ff8c1 (void)"
  , "{"
  , "  return &global_complex_float_flipped;"
  , "}"
  , "/* ExampleNothingget_global_complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_abdd562bd1b14921 (void)"
  , "{"
  , "  return &global_complex_double_flipped;"
  , "}"
  , "/* ExampleNothingget_global_Complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_02f701d4163d6ce7 (void)"
  , "{"
  , "  return &global_Complex_float;"
  , "}"
  , "/* ExampleNothingget_global_Complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_a6117bb5e7cacd17 (void)"
  , "{"
  , "  return &global_Complex_double;"
  , "}"
  , "/* ExampleNothingget_global_Complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_da2309480d364cee (void)"
  , "{"
  , "  return &global_Complex_float_flipped;"
  , "}"
  , "/* ExampleNothingget_global_Complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_467427dc59fbef50 (void)"
  , "{"
  , "  return &global_Complex_double_flipped;"
  , "}"
  , "/* ExampleNothingget_const_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex const *hs_bindgen_test_typescomplexhsb_complex_test_bb0fb18f3dfee47d (void)"
  , "{"
  , "  return &const_complex_float;"
  , "}"
  , "/* ExampleNothingget_const_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex const *hs_bindgen_test_typescomplexhsb_complex_test_f491f52e529a459a (void)"
  , "{"
  , "  return &const_complex_double;"
  , "}"
  , "/* ExampleNothingget_volatile_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_test_typescomplexhsb_complex_test_ecb5f4a0ccb7ee75 (void)"
  , "{"
  , "  return &volatile_complex_float;"
  , "}"
  , "/* ExampleNothingget_volatile_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_test_typescomplexhsb_complex_test_6b136090c38a69c4 (void)"
  , "{"
  , "  return &volatile_complex_double;"
  , "}"
  , "/* ExampleNothingget_complex_float_array_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_test_typescomplexhsb_complex_test_55b7fb104be53f70 (void))[10]"
  , "{"
  , "  return &complex_float_array;"
  , "}"
  , "/* ExampleNothingget_complex_double_array_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_test_typescomplexhsb_complex_test_0b63f3bda9243457 (void))[10]"
  , "{"
  , "  return &complex_double_array;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_global_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_69e4d4972011967b" hs_bindgen_test_typescomplexhsb_complex_test_69e4d4972011967b ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_ptr #-}

{-| __C declaration:__ @global_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:3:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_69e4d4972011967b

{-| __unique:__ @ExampleNothingget_global_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_c3633906ced5dab3" hs_bindgen_test_typescomplexhsb_complex_test_c3633906ced5dab3 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_ptr #-}

{-| __C declaration:__ @global_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:4:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_c3633906ced5dab3

{-| __unique:__ @ExampleNothingget_global_complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_7ef41813e25ff8c1" hs_bindgen_test_typescomplexhsb_complex_test_7ef41813e25ff8c1 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:6:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_7ef41813e25ff8c1

{-| __unique:__ @ExampleNothingget_global_complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_abdd562bd1b14921" hs_bindgen_test_typescomplexhsb_complex_test_abdd562bd1b14921 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:7:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_abdd562bd1b14921

{-| __unique:__ @ExampleNothingget_global_Complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_02f701d4163d6ce7" hs_bindgen_test_typescomplexhsb_complex_test_02f701d4163d6ce7 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_ptr #-}

{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:9:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_02f701d4163d6ce7

{-| __unique:__ @ExampleNothingget_global_Complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_a6117bb5e7cacd17" hs_bindgen_test_typescomplexhsb_complex_test_a6117bb5e7cacd17 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_ptr #-}

{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:10:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_a6117bb5e7cacd17

{-| __unique:__ @ExampleNothingget_global_Complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_da2309480d364cee" hs_bindgen_test_typescomplexhsb_complex_test_da2309480d364cee ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:12:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_da2309480d364cee

{-| __unique:__ @ExampleNothingget_global_Complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_467427dc59fbef50" hs_bindgen_test_typescomplexhsb_complex_test_467427dc59fbef50 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:13:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_467427dc59fbef50

{-| __unique:__ @ExampleNothingget_const_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_bb0fb18f3dfee47d" hs_bindgen_test_typescomplexhsb_complex_test_bb0fb18f3dfee47d ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE const_complex_float_ptr #-}

{-| __C declaration:__ @const_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:15:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
const_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_bb0fb18f3dfee47d

{-# NOINLINE const_complex_float #-}

const_complex_float :: Data.Complex.Complex FC.CFloat
const_complex_float =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_float_ptr)

{-| __unique:__ @ExampleNothingget_const_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_f491f52e529a459a" hs_bindgen_test_typescomplexhsb_complex_test_f491f52e529a459a ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE const_complex_double_ptr #-}

{-| __C declaration:__ @const_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:16:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
const_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_f491f52e529a459a

{-# NOINLINE const_complex_double #-}

const_complex_double :: Data.Complex.Complex FC.CDouble
const_complex_double =
  GHC.IO.Unsafe.unsafePerformIO (F.peek const_complex_double_ptr)

{-| __unique:__ @ExampleNothingget_volatile_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_ecb5f4a0ccb7ee75" hs_bindgen_test_typescomplexhsb_complex_test_ecb5f4a0ccb7ee75 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE volatile_complex_float_ptr #-}

{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:18:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
volatile_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_ecb5f4a0ccb7ee75

{-| __unique:__ @ExampleNothingget_volatile_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_6b136090c38a69c4" hs_bindgen_test_typescomplexhsb_complex_test_6b136090c38a69c4 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE volatile_complex_double_ptr #-}

{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:19:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
volatile_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_6b136090c38a69c4

{-| __unique:__ @ExampleNothingget_complex_float_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_55b7fb104be53f70" hs_bindgen_test_typescomplexhsb_complex_test_55b7fb104be53f70 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE complex_float_array_ptr #-}

{-| __C declaration:__ @complex_float_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:30:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_float_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat))
complex_float_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_55b7fb104be53f70

{-| __unique:__ @ExampleNothingget_complex_double_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_typescomplexhsb_complex_test_0b63f3bda9243457" hs_bindgen_test_typescomplexhsb_complex_test_0b63f3bda9243457 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE complex_double_array_ptr #-}

{-| __C declaration:__ @complex_double_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:31:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_double_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble))
complex_double_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_typescomplexhsb_complex_test_0b63f3bda9243457
