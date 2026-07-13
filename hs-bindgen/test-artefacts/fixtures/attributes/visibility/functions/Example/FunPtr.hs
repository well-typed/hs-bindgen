{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.f0
    , Example.FunPtr.f1
    , Example.FunPtr.f2
    , Example.FunPtr.f3
    , Example.FunPtr.f4
    , Example.FunPtr.f5
    , Example.FunPtr.f6
    , Example.FunPtr.f7
    , Example.FunPtr.f8
    , Example.FunPtr.f9
    , Example.FunPtr.f10
    , Example.FunPtr.f11
    , Example.FunPtr.f12
    , Example.FunPtr.f13
    , Example.FunPtr.f14
    , Example.FunPtr.f15
    , Example.FunPtr.f16
    , Example.FunPtr.f17
    , Example.FunPtr.f18
    , Example.FunPtr.f19
    , Example.FunPtr.f20
    , Example.FunPtr.f21
    , Example.FunPtr.f22
    , Example.FunPtr.f23
    , Example.FunPtr.f24
    , Example.FunPtr.f25
    , Example.FunPtr.f26
    , Example.FunPtr.f27
    , Example.FunPtr.f28
    , Example.FunPtr.f29
    )
  where

import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <attributes/visibility/functions.h>"
  , "/* test_attributesvisibilityfunction_Example_get_f0 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_b669c12e6c979397 (void)) (void)"
  , "{"
  , "  return &f0;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f1 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d3717f29814cc38e (void)) (void)"
  , "{"
  , "  return &f1;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f2 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9951e77d56815b80 (void)) (void)"
  , "{"
  , "  return &f2;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f3 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_735be5e937ab9ad0 (void)) (void)"
  , "{"
  , "  return &f3;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f4 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2e181350411c95e9 (void)) (void)"
  , "{"
  , "  return &f4;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f5 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_514428a6d0d70211 (void)) (void)"
  , "{"
  , "  return &f5;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f6 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9b7f8e653b947b35 (void)) (void)"
  , "{"
  , "  return &f6;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f7 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_53b66f2ee9d5bea3 (void)) (void)"
  , "{"
  , "  return &f7;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f8 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_90ce84ff3a158d95 (void)) (void)"
  , "{"
  , "  return &f8;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f9 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_bd91fda2207a62eb (void)) (void)"
  , "{"
  , "  return &f9;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f10 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f216131cd3bd6919 (void)) (void)"
  , "{"
  , "  return &f10;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f11 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d40c26619dcfad79 (void)) (void)"
  , "{"
  , "  return &f11;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f12 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_d507bba11a647ae8 (void)) (void)"
  , "{"
  , "  return &f12;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f13 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_a90968b5f0eb111f (void)) (void)"
  , "{"
  , "  return &f13;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f14 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f584fb1da48609e4 (void)) (void)"
  , "{"
  , "  return &f14;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f15 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1c5f8cf4562364d0 (void)) (void)"
  , "{"
  , "  return &f15;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f16 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6af855fd845ff731 (void)) (void)"
  , "{"
  , "  return &f16;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f17 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_c5aa3698b4ac9578 (void)) (void)"
  , "{"
  , "  return &f17;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f18 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_83b20ec12cdacad3 (void)) (void)"
  , "{"
  , "  return &f18;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f19 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_9530c7f467585e16 (void)) (void)"
  , "{"
  , "  return &f19;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f20 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_6b7091b3c8333945 (void)) (void)"
  , "{"
  , "  return &f20;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f21 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_58ed5b745dd5583b (void)) (void)"
  , "{"
  , "  return &f21;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f22 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_2fc8f17386d9d13f (void)) (void)"
  , "{"
  , "  return &f22;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f23 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_155c186bf2426f9c (void)) (void)"
  , "{"
  , "  return &f23;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f24 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_1c6a122ab1d7cef0 (void)) (void)"
  , "{"
  , "  return &f24;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f25 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_80ef1e1dba11ffdd (void)) (void)"
  , "{"
  , "  return &f25;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f26 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f7b63b778f9532da (void)) (void)"
  , "{"
  , "  return &f26;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f27 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_01ece11a3077bfd9 (void)) (void)"
  , "{"
  , "  return &f27;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f28 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_0bc3e07b1934c20b (void)) (void)"
  , "{"
  , "  return &f28;"
  , "}"
  , "/* test_attributesvisibilityfunction_Example_get_f29 */"
  , "__attribute__ ((const))"
  , "void (*hs_bindgen_f3734109e7f7d5ee (void)) (void)"
  , "{"
  , "  return &f29;"
  , "}"
  ]))

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f0@
foreign import ccall unsafe "hs_bindgen_b669c12e6c979397" hs_bindgen_b669c12e6c979397_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f0@
hs_bindgen_b669c12e6c979397 :: IO (BG.FunPtr (IO ()))
hs_bindgen_b669c12e6c979397 =
  BG.fromFFIType hs_bindgen_b669c12e6c979397_base

{-# NOINLINE f0 #-}
{-| __C declaration:__ @f0@

    __defined at:__ @attributes\/visibility\/functions.h 14:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f0 :: BG.FunPtr (IO ())
f0 = BG.unsafePerformIO hs_bindgen_b669c12e6c979397

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f1@
foreign import ccall unsafe "hs_bindgen_d3717f29814cc38e" hs_bindgen_d3717f29814cc38e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f1@
hs_bindgen_d3717f29814cc38e :: IO (BG.FunPtr (IO ()))
hs_bindgen_d3717f29814cc38e =
  BG.fromFFIType hs_bindgen_d3717f29814cc38e_base

{-# NOINLINE f1 #-}
{-| __C declaration:__ @f1@

    __defined at:__ @attributes\/visibility\/functions.h 15:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f1 :: BG.FunPtr (IO ())
f1 = BG.unsafePerformIO hs_bindgen_d3717f29814cc38e

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f2@
foreign import ccall unsafe "hs_bindgen_9951e77d56815b80" hs_bindgen_9951e77d56815b80_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f2@
hs_bindgen_9951e77d56815b80 :: IO (BG.FunPtr (IO ()))
hs_bindgen_9951e77d56815b80 =
  BG.fromFFIType hs_bindgen_9951e77d56815b80_base

{-# NOINLINE f2 #-}
{-| __C declaration:__ @f2@

    __defined at:__ @attributes\/visibility\/functions.h 16:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f2 :: BG.FunPtr (IO ())
f2 = BG.unsafePerformIO hs_bindgen_9951e77d56815b80

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f3@
foreign import ccall unsafe "hs_bindgen_735be5e937ab9ad0" hs_bindgen_735be5e937ab9ad0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f3@
hs_bindgen_735be5e937ab9ad0 :: IO (BG.FunPtr (IO ()))
hs_bindgen_735be5e937ab9ad0 =
  BG.fromFFIType hs_bindgen_735be5e937ab9ad0_base

{-# NOINLINE f3 #-}
{-| __C declaration:__ @f3@

    __defined at:__ @attributes\/visibility\/functions.h 17:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f3 :: BG.FunPtr (IO ())
f3 = BG.unsafePerformIO hs_bindgen_735be5e937ab9ad0

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f4@
foreign import ccall unsafe "hs_bindgen_2e181350411c95e9" hs_bindgen_2e181350411c95e9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f4@
hs_bindgen_2e181350411c95e9 :: IO (BG.FunPtr (IO ()))
hs_bindgen_2e181350411c95e9 =
  BG.fromFFIType hs_bindgen_2e181350411c95e9_base

{-# NOINLINE f4 #-}
{-| __C declaration:__ @f4@

    __defined at:__ @attributes\/visibility\/functions.h 18:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f4 :: BG.FunPtr (IO ())
f4 = BG.unsafePerformIO hs_bindgen_2e181350411c95e9

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f5@
foreign import ccall unsafe "hs_bindgen_514428a6d0d70211" hs_bindgen_514428a6d0d70211_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f5@
hs_bindgen_514428a6d0d70211 :: IO (BG.FunPtr (IO ()))
hs_bindgen_514428a6d0d70211 =
  BG.fromFFIType hs_bindgen_514428a6d0d70211_base

{-# NOINLINE f5 #-}
{-| __C declaration:__ @f5@

    __defined at:__ @attributes\/visibility\/functions.h 21:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f5 :: BG.FunPtr (IO ())
f5 = BG.unsafePerformIO hs_bindgen_514428a6d0d70211

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f6@
foreign import ccall unsafe "hs_bindgen_9b7f8e653b947b35" hs_bindgen_9b7f8e653b947b35_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f6@
hs_bindgen_9b7f8e653b947b35 :: IO (BG.FunPtr (IO ()))
hs_bindgen_9b7f8e653b947b35 =
  BG.fromFFIType hs_bindgen_9b7f8e653b947b35_base

{-# NOINLINE f6 #-}
{-| __C declaration:__ @f6@

    __defined at:__ @attributes\/visibility\/functions.h 22:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f6 :: BG.FunPtr (IO ())
f6 = BG.unsafePerformIO hs_bindgen_9b7f8e653b947b35

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f7@
foreign import ccall unsafe "hs_bindgen_53b66f2ee9d5bea3" hs_bindgen_53b66f2ee9d5bea3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f7@
hs_bindgen_53b66f2ee9d5bea3 :: IO (BG.FunPtr (IO ()))
hs_bindgen_53b66f2ee9d5bea3 =
  BG.fromFFIType hs_bindgen_53b66f2ee9d5bea3_base

{-# NOINLINE f7 #-}
{-| __C declaration:__ @f7@

    __defined at:__ @attributes\/visibility\/functions.h 23:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f7 :: BG.FunPtr (IO ())
f7 = BG.unsafePerformIO hs_bindgen_53b66f2ee9d5bea3

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f8@
foreign import ccall unsafe "hs_bindgen_90ce84ff3a158d95" hs_bindgen_90ce84ff3a158d95_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f8@
hs_bindgen_90ce84ff3a158d95 :: IO (BG.FunPtr (IO ()))
hs_bindgen_90ce84ff3a158d95 =
  BG.fromFFIType hs_bindgen_90ce84ff3a158d95_base

{-# NOINLINE f8 #-}
{-| __C declaration:__ @f8@

    __defined at:__ @attributes\/visibility\/functions.h 24:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f8 :: BG.FunPtr (IO ())
f8 = BG.unsafePerformIO hs_bindgen_90ce84ff3a158d95

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f9@
foreign import ccall unsafe "hs_bindgen_bd91fda2207a62eb" hs_bindgen_bd91fda2207a62eb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f9@
hs_bindgen_bd91fda2207a62eb :: IO (BG.FunPtr (IO ()))
hs_bindgen_bd91fda2207a62eb =
  BG.fromFFIType hs_bindgen_bd91fda2207a62eb_base

{-# NOINLINE f9 #-}
{-| __C declaration:__ @f9@

    __defined at:__ @attributes\/visibility\/functions.h 25:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f9 :: BG.FunPtr (IO ())
f9 = BG.unsafePerformIO hs_bindgen_bd91fda2207a62eb

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f10@
foreign import ccall unsafe "hs_bindgen_f216131cd3bd6919" hs_bindgen_f216131cd3bd6919_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f10@
hs_bindgen_f216131cd3bd6919 :: IO (BG.FunPtr (IO ()))
hs_bindgen_f216131cd3bd6919 =
  BG.fromFFIType hs_bindgen_f216131cd3bd6919_base

{-# NOINLINE f10 #-}
{-| __C declaration:__ @f10@

    __defined at:__ @attributes\/visibility\/functions.h 28:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f10 :: BG.FunPtr (IO ())
f10 = BG.unsafePerformIO hs_bindgen_f216131cd3bd6919

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f11@
foreign import ccall unsafe "hs_bindgen_d40c26619dcfad79" hs_bindgen_d40c26619dcfad79_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f11@
hs_bindgen_d40c26619dcfad79 :: IO (BG.FunPtr (IO ()))
hs_bindgen_d40c26619dcfad79 =
  BG.fromFFIType hs_bindgen_d40c26619dcfad79_base

{-# NOINLINE f11 #-}
{-| __C declaration:__ @f11@

    __defined at:__ @attributes\/visibility\/functions.h 29:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f11 :: BG.FunPtr (IO ())
f11 = BG.unsafePerformIO hs_bindgen_d40c26619dcfad79

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f12@
foreign import ccall unsafe "hs_bindgen_d507bba11a647ae8" hs_bindgen_d507bba11a647ae8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f12@
hs_bindgen_d507bba11a647ae8 :: IO (BG.FunPtr (IO ()))
hs_bindgen_d507bba11a647ae8 =
  BG.fromFFIType hs_bindgen_d507bba11a647ae8_base

{-# NOINLINE f12 #-}
{-| __C declaration:__ @f12@

    __defined at:__ @attributes\/visibility\/functions.h 30:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f12 :: BG.FunPtr (IO ())
f12 = BG.unsafePerformIO hs_bindgen_d507bba11a647ae8

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f13@
foreign import ccall unsafe "hs_bindgen_a90968b5f0eb111f" hs_bindgen_a90968b5f0eb111f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f13@
hs_bindgen_a90968b5f0eb111f :: IO (BG.FunPtr (IO ()))
hs_bindgen_a90968b5f0eb111f =
  BG.fromFFIType hs_bindgen_a90968b5f0eb111f_base

{-# NOINLINE f13 #-}
{-| __C declaration:__ @f13@

    __defined at:__ @attributes\/visibility\/functions.h 31:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f13 :: BG.FunPtr (IO ())
f13 = BG.unsafePerformIO hs_bindgen_a90968b5f0eb111f

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f14@
foreign import ccall unsafe "hs_bindgen_f584fb1da48609e4" hs_bindgen_f584fb1da48609e4_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f14@
hs_bindgen_f584fb1da48609e4 :: IO (BG.FunPtr (IO ()))
hs_bindgen_f584fb1da48609e4 =
  BG.fromFFIType hs_bindgen_f584fb1da48609e4_base

{-# NOINLINE f14 #-}
{-| __C declaration:__ @f14@

    __defined at:__ @attributes\/visibility\/functions.h 32:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f14 :: BG.FunPtr (IO ())
f14 = BG.unsafePerformIO hs_bindgen_f584fb1da48609e4

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f15@
foreign import ccall unsafe "hs_bindgen_1c5f8cf4562364d0" hs_bindgen_1c5f8cf4562364d0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f15@
hs_bindgen_1c5f8cf4562364d0 :: IO (BG.FunPtr (IO ()))
hs_bindgen_1c5f8cf4562364d0 =
  BG.fromFFIType hs_bindgen_1c5f8cf4562364d0_base

{-# NOINLINE f15 #-}
{-| __C declaration:__ @f15@

    __defined at:__ @attributes\/visibility\/functions.h 35:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f15 :: BG.FunPtr (IO ())
f15 = BG.unsafePerformIO hs_bindgen_1c5f8cf4562364d0

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f16@
foreign import ccall unsafe "hs_bindgen_6af855fd845ff731" hs_bindgen_6af855fd845ff731_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f16@
hs_bindgen_6af855fd845ff731 :: IO (BG.FunPtr (IO ()))
hs_bindgen_6af855fd845ff731 =
  BG.fromFFIType hs_bindgen_6af855fd845ff731_base

{-# NOINLINE f16 #-}
{-| __C declaration:__ @f16@

    __defined at:__ @attributes\/visibility\/functions.h 36:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f16 :: BG.FunPtr (IO ())
f16 = BG.unsafePerformIO hs_bindgen_6af855fd845ff731

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f17@
foreign import ccall unsafe "hs_bindgen_c5aa3698b4ac9578" hs_bindgen_c5aa3698b4ac9578_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f17@
hs_bindgen_c5aa3698b4ac9578 :: IO (BG.FunPtr (IO ()))
hs_bindgen_c5aa3698b4ac9578 =
  BG.fromFFIType hs_bindgen_c5aa3698b4ac9578_base

{-# NOINLINE f17 #-}
{-| __C declaration:__ @f17@

    __defined at:__ @attributes\/visibility\/functions.h 37:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f17 :: BG.FunPtr (IO ())
f17 = BG.unsafePerformIO hs_bindgen_c5aa3698b4ac9578

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f18@
foreign import ccall unsafe "hs_bindgen_83b20ec12cdacad3" hs_bindgen_83b20ec12cdacad3_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f18@
hs_bindgen_83b20ec12cdacad3 :: IO (BG.FunPtr (IO ()))
hs_bindgen_83b20ec12cdacad3 =
  BG.fromFFIType hs_bindgen_83b20ec12cdacad3_base

{-# NOINLINE f18 #-}
{-| __C declaration:__ @f18@

    __defined at:__ @attributes\/visibility\/functions.h 38:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f18 :: BG.FunPtr (IO ())
f18 = BG.unsafePerformIO hs_bindgen_83b20ec12cdacad3

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f19@
foreign import ccall unsafe "hs_bindgen_9530c7f467585e16" hs_bindgen_9530c7f467585e16_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f19@
hs_bindgen_9530c7f467585e16 :: IO (BG.FunPtr (IO ()))
hs_bindgen_9530c7f467585e16 =
  BG.fromFFIType hs_bindgen_9530c7f467585e16_base

{-# NOINLINE f19 #-}
{-| __C declaration:__ @f19@

    __defined at:__ @attributes\/visibility\/functions.h 39:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f19 :: BG.FunPtr (IO ())
f19 = BG.unsafePerformIO hs_bindgen_9530c7f467585e16

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f20@
foreign import ccall unsafe "hs_bindgen_6b7091b3c8333945" hs_bindgen_6b7091b3c8333945_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f20@
hs_bindgen_6b7091b3c8333945 :: IO (BG.FunPtr (IO ()))
hs_bindgen_6b7091b3c8333945 =
  BG.fromFFIType hs_bindgen_6b7091b3c8333945_base

{-# NOINLINE f20 #-}
{-| __C declaration:__ @f20@

    __defined at:__ @attributes\/visibility\/functions.h 42:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f20 :: BG.FunPtr (IO ())
f20 = BG.unsafePerformIO hs_bindgen_6b7091b3c8333945

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f21@
foreign import ccall unsafe "hs_bindgen_58ed5b745dd5583b" hs_bindgen_58ed5b745dd5583b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f21@
hs_bindgen_58ed5b745dd5583b :: IO (BG.FunPtr (IO ()))
hs_bindgen_58ed5b745dd5583b =
  BG.fromFFIType hs_bindgen_58ed5b745dd5583b_base

{-# NOINLINE f21 #-}
{-| __C declaration:__ @f21@

    __defined at:__ @attributes\/visibility\/functions.h 43:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f21 :: BG.FunPtr (IO ())
f21 = BG.unsafePerformIO hs_bindgen_58ed5b745dd5583b

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f22@
foreign import ccall unsafe "hs_bindgen_2fc8f17386d9d13f" hs_bindgen_2fc8f17386d9d13f_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f22@
hs_bindgen_2fc8f17386d9d13f :: IO (BG.FunPtr (IO ()))
hs_bindgen_2fc8f17386d9d13f =
  BG.fromFFIType hs_bindgen_2fc8f17386d9d13f_base

{-# NOINLINE f22 #-}
{-| __C declaration:__ @f22@

    __defined at:__ @attributes\/visibility\/functions.h 44:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f22 :: BG.FunPtr (IO ())
f22 = BG.unsafePerformIO hs_bindgen_2fc8f17386d9d13f

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f23@
foreign import ccall unsafe "hs_bindgen_155c186bf2426f9c" hs_bindgen_155c186bf2426f9c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f23@
hs_bindgen_155c186bf2426f9c :: IO (BG.FunPtr (IO ()))
hs_bindgen_155c186bf2426f9c =
  BG.fromFFIType hs_bindgen_155c186bf2426f9c_base

{-# NOINLINE f23 #-}
{-| __C declaration:__ @f23@

    __defined at:__ @attributes\/visibility\/functions.h 45:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f23 :: BG.FunPtr (IO ())
f23 = BG.unsafePerformIO hs_bindgen_155c186bf2426f9c

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f24@
foreign import ccall unsafe "hs_bindgen_1c6a122ab1d7cef0" hs_bindgen_1c6a122ab1d7cef0_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f24@
hs_bindgen_1c6a122ab1d7cef0 :: IO (BG.FunPtr (IO ()))
hs_bindgen_1c6a122ab1d7cef0 =
  BG.fromFFIType hs_bindgen_1c6a122ab1d7cef0_base

{-# NOINLINE f24 #-}
{-| __C declaration:__ @f24@

    __defined at:__ @attributes\/visibility\/functions.h 46:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f24 :: BG.FunPtr (IO ())
f24 = BG.unsafePerformIO hs_bindgen_1c6a122ab1d7cef0

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f25@
foreign import ccall unsafe "hs_bindgen_80ef1e1dba11ffdd" hs_bindgen_80ef1e1dba11ffdd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f25@
hs_bindgen_80ef1e1dba11ffdd :: IO (BG.FunPtr (IO ()))
hs_bindgen_80ef1e1dba11ffdd =
  BG.fromFFIType hs_bindgen_80ef1e1dba11ffdd_base

{-# NOINLINE f25 #-}
{-| __C declaration:__ @f25@

    __defined at:__ @attributes\/visibility\/functions.h 49:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f25 :: BG.FunPtr (IO ())
f25 = BG.unsafePerformIO hs_bindgen_80ef1e1dba11ffdd

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f26@
foreign import ccall unsafe "hs_bindgen_f7b63b778f9532da" hs_bindgen_f7b63b778f9532da_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f26@
hs_bindgen_f7b63b778f9532da :: IO (BG.FunPtr (IO ()))
hs_bindgen_f7b63b778f9532da =
  BG.fromFFIType hs_bindgen_f7b63b778f9532da_base

{-# NOINLINE f26 #-}
{-| __C declaration:__ @f26@

    __defined at:__ @attributes\/visibility\/functions.h 50:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f26 :: BG.FunPtr (IO ())
f26 = BG.unsafePerformIO hs_bindgen_f7b63b778f9532da

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f27@
foreign import ccall unsafe "hs_bindgen_01ece11a3077bfd9" hs_bindgen_01ece11a3077bfd9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f27@
hs_bindgen_01ece11a3077bfd9 :: IO (BG.FunPtr (IO ()))
hs_bindgen_01ece11a3077bfd9 =
  BG.fromFFIType hs_bindgen_01ece11a3077bfd9_base

{-# NOINLINE f27 #-}
{-| __C declaration:__ @f27@

    __defined at:__ @attributes\/visibility\/functions.h 51:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f27 :: BG.FunPtr (IO ())
f27 = BG.unsafePerformIO hs_bindgen_01ece11a3077bfd9

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f28@
foreign import ccall unsafe "hs_bindgen_0bc3e07b1934c20b" hs_bindgen_0bc3e07b1934c20b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f28@
hs_bindgen_0bc3e07b1934c20b :: IO (BG.FunPtr (IO ()))
hs_bindgen_0bc3e07b1934c20b =
  BG.fromFFIType hs_bindgen_0bc3e07b1934c20b_base

{-# NOINLINE f28 #-}
{-| __C declaration:__ @f28@

    __defined at:__ @attributes\/visibility\/functions.h 52:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f28 :: BG.FunPtr (IO ())
f28 = BG.unsafePerformIO hs_bindgen_0bc3e07b1934c20b

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f29@
foreign import ccall unsafe "hs_bindgen_f3734109e7f7d5ee" hs_bindgen_f3734109e7f7d5ee_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_attributesvisibilityfunction_Example_get_f29@
hs_bindgen_f3734109e7f7d5ee :: IO (BG.FunPtr (IO ()))
hs_bindgen_f3734109e7f7d5ee =
  BG.fromFFIType hs_bindgen_f3734109e7f7d5ee_base

{-# NOINLINE f29 #-}
{-| __C declaration:__ @f29@

    __defined at:__ @attributes\/visibility\/functions.h 53:56@

    __exported by:__ @attributes\/visibility\/functions.h@
-}
f29 :: BG.FunPtr (IO ())
f29 = BG.unsafePerformIO hs_bindgen_f3734109e7f7d5ee
