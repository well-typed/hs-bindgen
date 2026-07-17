{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.global_complex_float
    , Example.Global.global_complex_double
    , Example.Global.global_complex_float_flipped
    , Example.Global.global_complex_double_flipped
    , Example.Global.global_Complex_float
    , Example.Global.global_Complex_double
    , Example.Global.global_Complex_float_flipped
    , Example.Global.global_Complex_double_flipped
    , Example.Global.const_complex_float
    , Example.Global.const_complex_double
    , Example.Global.volatile_complex_float
    , Example.Global.volatile_complex_double
    , Example.Global.complex_float_array
    , Example.Global.complex_double_array
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <types/complex/hsb_complex_test.h>"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_209e61216da37381 (void)"
  , "{"
  , "  return &global_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_4d0950cebe4897a7 (void)"
  , "{"
  , "  return &global_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_7e9edc1d187b0799 (void)"
  , "{"
  , "  return &global_complex_float_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_fe8783b71a4e1d3c (void)"
  , "{"
  , "  return &global_complex_double_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_0aa0098565c13195 (void)"
  , "{"
  , "  return &global_Complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_5f43cb32dc9ef6af (void)"
  , "{"
  , "  return &global_Complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_40d5347e1c2ae46a (void)"
  , "{"
  , "  return &global_Complex_float_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_0e377cedb96a3ba4 (void)"
  , "{"
  , "  return &global_Complex_double_flipped;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_const_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex const *hs_bindgen_d94307879b0a380f (void)"
  , "{"
  , "  return &const_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_const_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex const *hs_bindgen_de1422a11860ada5 (void)"
  , "{"
  , "  return &const_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_volatile_complex_float */"
  , "__attribute__ ((const))"
  , "float _Complex *hs_bindgen_569a907813a0dadd (void)"
  , "{"
  , "  return &volatile_complex_float;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_volatile_complex_double */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_e86c9014eb507c96 (void)"
  , "{"
  , "  return &volatile_complex_double;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_complex_float_array */"
  , "__attribute__ ((const))"
  , "float _Complex (*hs_bindgen_b548dda6b5993793 (void))[10]"
  , "{"
  , "  return &complex_float_array;"
  , "}"
  , "/* test_typescomplexhsb_complex_test_Example_get_complex_double_array */"
  , "__attribute__ ((const))"
  , "double _Complex (*hs_bindgen_c56913da40952669 (void))[10]"
  , "{"
  , "  return &complex_double_array;"
  , "}"
  ]))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float@
foreign import ccall unsafe "hs_bindgen_209e61216da37381" hs_bindgen_209e61216da37381_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float@
hs_bindgen_209e61216da37381 :: IO (BG.Ptr (BG.Complex BG.CFloat))
hs_bindgen_209e61216da37381 =
  BG.fromFFIType hs_bindgen_209e61216da37381_base

{-# NOINLINE global_complex_float #-}
{-| __C declaration:__ @global_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 3:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float :: BG.Ptr (BG.Complex BG.CFloat)
global_complex_float =
  BG.unsafePerformIO hs_bindgen_209e61216da37381

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double@
foreign import ccall unsafe "hs_bindgen_4d0950cebe4897a7" hs_bindgen_4d0950cebe4897a7_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double@
hs_bindgen_4d0950cebe4897a7 :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_4d0950cebe4897a7 =
  BG.fromFFIType hs_bindgen_4d0950cebe4897a7_base

{-# NOINLINE global_complex_double #-}
{-| __C declaration:__ @global_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 4:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double :: BG.Ptr (BG.Complex BG.CDouble)
global_complex_double =
  BG.unsafePerformIO hs_bindgen_4d0950cebe4897a7

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped@
foreign import ccall unsafe "hs_bindgen_7e9edc1d187b0799" hs_bindgen_7e9edc1d187b0799_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped@
hs_bindgen_7e9edc1d187b0799 :: IO (BG.Ptr (BG.Complex BG.CFloat))
hs_bindgen_7e9edc1d187b0799 =
  BG.fromFFIType hs_bindgen_7e9edc1d187b0799_base

{-# NOINLINE global_complex_float_flipped #-}
{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 6:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_flipped :: BG.Ptr (BG.Complex BG.CFloat)
global_complex_float_flipped =
  BG.unsafePerformIO hs_bindgen_7e9edc1d187b0799

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped@
foreign import ccall unsafe "hs_bindgen_fe8783b71a4e1d3c" hs_bindgen_fe8783b71a4e1d3c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped@
hs_bindgen_fe8783b71a4e1d3c :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_fe8783b71a4e1d3c =
  BG.fromFFIType hs_bindgen_fe8783b71a4e1d3c_base

{-# NOINLINE global_complex_double_flipped #-}
{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 7:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_flipped :: BG.Ptr (BG.Complex BG.CDouble)
global_complex_double_flipped =
  BG.unsafePerformIO hs_bindgen_fe8783b71a4e1d3c

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float@
foreign import ccall unsafe "hs_bindgen_0aa0098565c13195" hs_bindgen_0aa0098565c13195_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float@
hs_bindgen_0aa0098565c13195 :: IO (BG.Ptr (BG.Complex BG.CFloat))
hs_bindgen_0aa0098565c13195 =
  BG.fromFFIType hs_bindgen_0aa0098565c13195_base

{-# NOINLINE global_Complex_float #-}
{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 9:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float :: BG.Ptr (BG.Complex BG.CFloat)
global_Complex_float =
  BG.unsafePerformIO hs_bindgen_0aa0098565c13195

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double@
foreign import ccall unsafe "hs_bindgen_5f43cb32dc9ef6af" hs_bindgen_5f43cb32dc9ef6af_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double@
hs_bindgen_5f43cb32dc9ef6af :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_5f43cb32dc9ef6af =
  BG.fromFFIType hs_bindgen_5f43cb32dc9ef6af_base

{-# NOINLINE global_Complex_double #-}
{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 10:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double :: BG.Ptr (BG.Complex BG.CDouble)
global_Complex_double =
  BG.unsafePerformIO hs_bindgen_5f43cb32dc9ef6af

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped@
foreign import ccall unsafe "hs_bindgen_40d5347e1c2ae46a" hs_bindgen_40d5347e1c2ae46a_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped@
hs_bindgen_40d5347e1c2ae46a :: IO (BG.Ptr (BG.Complex BG.CFloat))
hs_bindgen_40d5347e1c2ae46a =
  BG.fromFFIType hs_bindgen_40d5347e1c2ae46a_base

{-# NOINLINE global_Complex_float_flipped #-}
{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 12:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_flipped :: BG.Ptr (BG.Complex BG.CFloat)
global_Complex_float_flipped =
  BG.unsafePerformIO hs_bindgen_40d5347e1c2ae46a

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped@
foreign import ccall unsafe "hs_bindgen_0e377cedb96a3ba4" hs_bindgen_0e377cedb96a3ba4_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped@
hs_bindgen_0e377cedb96a3ba4 :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_0e377cedb96a3ba4 =
  BG.fromFFIType hs_bindgen_0e377cedb96a3ba4_base

{-# NOINLINE global_Complex_double_flipped #-}
{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 13:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_flipped :: BG.Ptr (BG.Complex BG.CDouble)
global_Complex_double_flipped =
  BG.unsafePerformIO hs_bindgen_0e377cedb96a3ba4

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_float@
foreign import ccall unsafe "hs_bindgen_d94307879b0a380f" hs_bindgen_d94307879b0a380f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_float@
hs_bindgen_d94307879b0a380f :: IO (PtrConst.PtrConst (BG.Complex BG.CFloat))
hs_bindgen_d94307879b0a380f =
  BG.fromFFIType hs_bindgen_d94307879b0a380f_base

{-# NOINLINE hs_bindgen_554ede1a618d726f #-}
{-| __C declaration:__ @const_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 15:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@

    __unique:__ @test_typescomplexhsb_complex_test_Example_const_complex_float@
-}
hs_bindgen_554ede1a618d726f :: PtrConst.PtrConst (BG.Complex BG.CFloat)
hs_bindgen_554ede1a618d726f =
  BG.unsafePerformIO hs_bindgen_d94307879b0a380f

{-# NOINLINE const_complex_float #-}
const_complex_float :: BG.Complex BG.CFloat
const_complex_float =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_554ede1a618d726f)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_double@
foreign import ccall unsafe "hs_bindgen_de1422a11860ada5" hs_bindgen_de1422a11860ada5_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_double@
hs_bindgen_de1422a11860ada5 :: IO (PtrConst.PtrConst (BG.Complex BG.CDouble))
hs_bindgen_de1422a11860ada5 =
  BG.fromFFIType hs_bindgen_de1422a11860ada5_base

{-# NOINLINE hs_bindgen_cf751023874b527f #-}
{-| __C declaration:__ @const_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 16:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@

    __unique:__ @test_typescomplexhsb_complex_test_Example_const_complex_double@
-}
hs_bindgen_cf751023874b527f :: PtrConst.PtrConst (BG.Complex BG.CDouble)
hs_bindgen_cf751023874b527f =
  BG.unsafePerformIO hs_bindgen_de1422a11860ada5

{-# NOINLINE const_complex_double #-}
const_complex_double :: BG.Complex BG.CDouble
const_complex_double =
  BG.unsafePerformIO (PtrConst.peek hs_bindgen_cf751023874b527f)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_float@
foreign import ccall unsafe "hs_bindgen_569a907813a0dadd" hs_bindgen_569a907813a0dadd_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_float@
hs_bindgen_569a907813a0dadd :: IO (BG.Ptr (BG.Complex BG.CFloat))
hs_bindgen_569a907813a0dadd =
  BG.fromFFIType hs_bindgen_569a907813a0dadd_base

{-# NOINLINE volatile_complex_float #-}
{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 18:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_float :: BG.Ptr (BG.Complex BG.CFloat)
volatile_complex_float =
  BG.unsafePerformIO hs_bindgen_569a907813a0dadd

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_double@
foreign import ccall unsafe "hs_bindgen_e86c9014eb507c96" hs_bindgen_e86c9014eb507c96_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_double@
hs_bindgen_e86c9014eb507c96 :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_e86c9014eb507c96 =
  BG.fromFFIType hs_bindgen_e86c9014eb507c96_base

{-# NOINLINE volatile_complex_double #-}
{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 19:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_double :: BG.Ptr (BG.Complex BG.CDouble)
volatile_complex_double =
  BG.unsafePerformIO hs_bindgen_e86c9014eb507c96

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_float_array@
foreign import ccall unsafe "hs_bindgen_b548dda6b5993793" hs_bindgen_b548dda6b5993793_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_float_array@
hs_bindgen_b548dda6b5993793 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Complex BG.CFloat)))
hs_bindgen_b548dda6b5993793 =
  BG.fromFFIType hs_bindgen_b548dda6b5993793_base

{-# NOINLINE complex_float_array #-}
{-| __C declaration:__ @complex_float_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h 30:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_float_array :: BG.Ptr (CA.ConstantArray 10 (BG.Complex BG.CFloat))
complex_float_array =
  BG.unsafePerformIO hs_bindgen_b548dda6b5993793

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_double_array@
foreign import ccall unsafe "hs_bindgen_c56913da40952669" hs_bindgen_c56913da40952669_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_double_array@
hs_bindgen_c56913da40952669 :: IO (BG.Ptr (CA.ConstantArray 10 (BG.Complex BG.CDouble)))
hs_bindgen_c56913da40952669 =
  BG.fromFFIType hs_bindgen_c56913da40952669_base

{-# NOINLINE complex_double_array #-}
{-| __C declaration:__ @complex_double_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h 31:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_double_array :: BG.Ptr (CA.ConstantArray 10 (BG.Complex BG.CDouble))
complex_double_array =
  BG.unsafePerformIO hs_bindgen_c56913da40952669
