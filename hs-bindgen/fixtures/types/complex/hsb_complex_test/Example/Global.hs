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
import qualified HsBindgen.Runtime.CAPI
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import Data.Void (Void)
import Prelude (IO)

$(HsBindgen.Runtime.CAPI.addCSource (HsBindgen.Runtime.CAPI.unlines
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
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float@
hs_bindgen_209e61216da37381 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))
hs_bindgen_209e61216da37381 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_209e61216da37381_base

{-# NOINLINE global_complex_float #-}
{-| __C declaration:__ @global_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 3:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_209e61216da37381

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double@
foreign import ccall unsafe "hs_bindgen_4d0950cebe4897a7" hs_bindgen_4d0950cebe4897a7_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double@
hs_bindgen_4d0950cebe4897a7 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))
hs_bindgen_4d0950cebe4897a7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4d0950cebe4897a7_base

{-# NOINLINE global_complex_double #-}
{-| __C declaration:__ @global_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 4:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4d0950cebe4897a7

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped@
foreign import ccall unsafe "hs_bindgen_7e9edc1d187b0799" hs_bindgen_7e9edc1d187b0799_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_float_flipped@
hs_bindgen_7e9edc1d187b0799 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))
hs_bindgen_7e9edc1d187b0799 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7e9edc1d187b0799_base

{-# NOINLINE global_complex_float_flipped #-}
{-| __C declaration:__ @global_complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 6:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_float_flipped :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_complex_float_flipped =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7e9edc1d187b0799

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped@
foreign import ccall unsafe "hs_bindgen_fe8783b71a4e1d3c" hs_bindgen_fe8783b71a4e1d3c_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_complex_double_flipped@
hs_bindgen_fe8783b71a4e1d3c :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))
hs_bindgen_fe8783b71a4e1d3c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_fe8783b71a4e1d3c_base

{-# NOINLINE global_complex_double_flipped #-}
{-| __C declaration:__ @global_complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 7:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_complex_double_flipped :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_complex_double_flipped =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fe8783b71a4e1d3c

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float@
foreign import ccall unsafe "hs_bindgen_0aa0098565c13195" hs_bindgen_0aa0098565c13195_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float@
hs_bindgen_0aa0098565c13195 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))
hs_bindgen_0aa0098565c13195 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0aa0098565c13195_base

{-# NOINLINE global_Complex_float #-}
{-| __C declaration:__ @global_Complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 9:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0aa0098565c13195

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double@
foreign import ccall unsafe "hs_bindgen_5f43cb32dc9ef6af" hs_bindgen_5f43cb32dc9ef6af_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double@
hs_bindgen_5f43cb32dc9ef6af :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))
hs_bindgen_5f43cb32dc9ef6af =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_5f43cb32dc9ef6af_base

{-# NOINLINE global_Complex_double #-}
{-| __C declaration:__ @global_Complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 10:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5f43cb32dc9ef6af

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped@
foreign import ccall unsafe "hs_bindgen_40d5347e1c2ae46a" hs_bindgen_40d5347e1c2ae46a_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_float_flipped@
hs_bindgen_40d5347e1c2ae46a :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))
hs_bindgen_40d5347e1c2ae46a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_40d5347e1c2ae46a_base

{-# NOINLINE global_Complex_float_flipped #-}
{-| __C declaration:__ @global_Complex_float_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 12:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_float_flipped :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
global_Complex_float_flipped =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_40d5347e1c2ae46a

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped@
foreign import ccall unsafe "hs_bindgen_0e377cedb96a3ba4" hs_bindgen_0e377cedb96a3ba4_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_global_Complex_double_flipped@
hs_bindgen_0e377cedb96a3ba4 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))
hs_bindgen_0e377cedb96a3ba4 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0e377cedb96a3ba4_base

{-# NOINLINE global_Complex_double_flipped #-}
{-| __C declaration:__ @global_Complex_double_flipped@

    __defined at:__ @types\/complex\/hsb_complex_test.h 13:24@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
global_Complex_double_flipped :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
global_Complex_double_flipped =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0e377cedb96a3ba4

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_float@
foreign import ccall unsafe "hs_bindgen_d94307879b0a380f" hs_bindgen_d94307879b0a380f_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_float@
hs_bindgen_d94307879b0a380f :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CFloat))
hs_bindgen_d94307879b0a380f =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d94307879b0a380f_base

{-# NOINLINE hs_bindgen_554ede1a618d726f #-}
{-| __C declaration:__ @const_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 15:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@

    __unique:__ @test_typescomplexhsb_complex_test_Example_const_complex_float@
-}
hs_bindgen_554ede1a618d726f :: HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CFloat)
hs_bindgen_554ede1a618d726f =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d94307879b0a380f

{-# NOINLINE const_complex_float #-}
const_complex_float :: Data.Complex.Complex FC.CFloat
const_complex_float =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_554ede1a618d726f))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_double@
foreign import ccall unsafe "hs_bindgen_de1422a11860ada5" hs_bindgen_de1422a11860ada5_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_const_complex_double@
hs_bindgen_de1422a11860ada5 :: IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CDouble))
hs_bindgen_de1422a11860ada5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_de1422a11860ada5_base

{-# NOINLINE hs_bindgen_cf751023874b527f #-}
{-| __C declaration:__ @const_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 16:29@

    __exported by:__ @types\/complex\/hsb_complex_test.h@

    __unique:__ @test_typescomplexhsb_complex_test_Example_const_complex_double@
-}
hs_bindgen_cf751023874b527f :: HsBindgen.Runtime.ConstPtr.ConstPtr (Data.Complex.Complex FC.CDouble)
hs_bindgen_cf751023874b527f =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_de1422a11860ada5

{-# NOINLINE const_complex_double #-}
const_complex_double :: Data.Complex.Complex FC.CDouble
const_complex_double =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_cf751023874b527f))

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_float@
foreign import ccall unsafe "hs_bindgen_569a907813a0dadd" hs_bindgen_569a907813a0dadd_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_float@
hs_bindgen_569a907813a0dadd :: IO (Ptr.Ptr (Data.Complex.Complex FC.CFloat))
hs_bindgen_569a907813a0dadd =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_569a907813a0dadd_base

{-# NOINLINE volatile_complex_float #-}
{-| __C declaration:__ @volatile_complex_float@

    __defined at:__ @types\/complex\/hsb_complex_test.h 18:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_float :: Ptr.Ptr (Data.Complex.Complex FC.CFloat)
volatile_complex_float =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_569a907813a0dadd

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_double@
foreign import ccall unsafe "hs_bindgen_e86c9014eb507c96" hs_bindgen_e86c9014eb507c96_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_volatile_complex_double@
hs_bindgen_e86c9014eb507c96 :: IO (Ptr.Ptr (Data.Complex.Complex FC.CDouble))
hs_bindgen_e86c9014eb507c96 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e86c9014eb507c96_base

{-# NOINLINE volatile_complex_double #-}
{-| __C declaration:__ @volatile_complex_double@

    __defined at:__ @types\/complex\/hsb_complex_test.h 19:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
volatile_complex_double :: Ptr.Ptr (Data.Complex.Complex FC.CDouble)
volatile_complex_double =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e86c9014eb507c96

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_float_array@
foreign import ccall unsafe "hs_bindgen_b548dda6b5993793" hs_bindgen_b548dda6b5993793_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_float_array@
hs_bindgen_b548dda6b5993793 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat)))
hs_bindgen_b548dda6b5993793 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b548dda6b5993793_base

{-# NOINLINE complex_float_array #-}
{-| __C declaration:__ @complex_float_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h 30:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_float_array :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CFloat))
complex_float_array =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b548dda6b5993793

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_double_array@
foreign import ccall unsafe "hs_bindgen_c56913da40952669" hs_bindgen_c56913da40952669_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_typescomplexhsb_complex_test_Example_get_complex_double_array@
hs_bindgen_c56913da40952669 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble)))
hs_bindgen_c56913da40952669 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c56913da40952669_base

{-# NOINLINE complex_double_array #-}
{-| __C declaration:__ @complex_double_array@

    __defined at:__ @types\/complex\/hsb_complex_test.h 31:23@

    __exported by:__ @types\/complex\/hsb_complex_test.h@
-}
complex_double_array :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 10) (Data.Complex.Complex FC.CDouble))
complex_double_array =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c56913da40952669
