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
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_a6dcbf0ebef057c9 (void)"
  , "{"
  , "  return &global_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_6102571a73986812 (void)"
  , "{"
  , "  return &global_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_1b0a02397e2ea2f6 (void)"
  , "{"
  , "  return &global_complex_float_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_4be2464f88314410 (void)"
  , "{"
  , "  return &global_complex_double_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_aa07323f7398ff97 (void)"
  , "{"
  , "  return &global_Complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_190b07a847b36556 (void)"
  , "{"
  , "  return &global_Complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_017435f1022a672c (void)"
  , "{"
  , "  return &global_Complex_float_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_aec6991fbd3ffdbb (void)"
  , "{"
  , "  return &global_Complex_double_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_const_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex const *hs_bindgen_3ea97dc5fdb27263 (void)"
  , "{"
  , "  return &const_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_const_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex const *hs_bindgen_086209fa0eb9a3ee (void)"
  , "{"
  , "  return &const_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_volatile_complex_float_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_00177fb0da38717b (void)"
  , "{"
  , "  return &volatile_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_volatile_complex_double_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_72f5727716adf5ac (void)"
  , "{"
  , "  return &volatile_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_complex_float_array_ptr */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_229c51ee9572efe8 (void))[10]"
  , "{"
  , "  return &complex_float_array;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_complex_double_array_ptr */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_b2b11d22608bbfec (void))[10]"
  , "{"
  , "  return &complex_double_array;"
  , "}"
  ]))

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_a6dcbf0ebef057c9" hs_bindgen_a6dcbf0ebef057c9 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_ptr #-}

{-| __C declaration:__ @global_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:3:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a6dcbf0ebef057c9

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_6102571a73986812" hs_bindgen_6102571a73986812 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_ptr #-}

{-| __C declaration:__ @global_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:4:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6102571a73986812

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_1b0a02397e2ea2f6" hs_bindgen_1b0a02397e2ea2f6 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:6:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1b0a02397e2ea2f6

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_4be2464f88314410" hs_bindgen_4be2464f88314410 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:7:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4be2464f88314410

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_aa07323f7398ff97" hs_bindgen_aa07323f7398ff97 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_ptr #-}

{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:9:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aa07323f7398ff97

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_190b07a847b36556" hs_bindgen_190b07a847b36556 ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_ptr #-}

{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:10:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_190b07a847b36556

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_017435f1022a672c" hs_bindgen_017435f1022a672c ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE global_Complex_float_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:12:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_017435f1022a672c

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped_ptr@
-}
foreign import ccall unsafe "hs_bindgen_aec6991fbd3ffdbb" hs_bindgen_aec6991fbd3ffdbb ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE global_Complex_double_flipped_ptr #-}

{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h:13:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_flipped_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_flipped_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_aec6991fbd3ffdbb

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_3ea97dc5fdb27263" hs_bindgen_3ea97dc5fdb27263 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE const_complex_float_ptr #-}

{-| __C declaration:__ @const_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:15:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_float_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CFloat)
const_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ea97dc5fdb27263

{-# NOINLINE const_complex_float #-}

const_complex_float :: Data.Complex.Complex FC.CFloat
const_complex_float =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr const_complex_float_ptr))

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_086209fa0eb9a3ee" hs_bindgen_086209fa0eb9a3ee ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE const_complex_double_ptr #-}

{-| __C declaration:__ @const_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:16:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
const_complex_double_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CDouble)
const_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_086209fa0eb9a3ee

{-# NOINLINE const_complex_double #-}

const_complex_double :: Data.Complex.Complex FC.CDouble
const_complex_double =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr const_complex_double_ptr))

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_float_ptr@
-}
foreign import ccall unsafe "hs_bindgen_00177fb0da38717b" hs_bindgen_00177fb0da38717b ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))

{-# NOINLINE volatile_complex_float_ptr #-}

{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h:18:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_float_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
volatile_complex_float_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_00177fb0da38717b

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_double_ptr@
-}
foreign import ccall unsafe "hs_bindgen_72f5727716adf5ac" hs_bindgen_72f5727716adf5ac ::
     IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))

{-# NOINLINE volatile_complex_double_ptr #-}

{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h:19:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_double_ptr :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
volatile_complex_double_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_72f5727716adf5ac

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_float_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_229c51ee9572efe8" hs_bindgen_229c51ee9572efe8 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat)))

{-# NOINLINE complex_float_array_ptr #-}

{-| __C declaration:__ @complex_float_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:30:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_float_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat))
complex_float_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_229c51ee9572efe8

{-| __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_double_array_ptr@
-}
foreign import ccall unsafe "hs_bindgen_b2b11d22608bbfec" hs_bindgen_b2b11d22608bbfec ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble)))

{-# NOINLINE complex_double_array_ptr #-}

{-| __C declaration:__ @complex_double_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h:31:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_double_array_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble))
complex_double_array_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b2b11d22608bbfec
