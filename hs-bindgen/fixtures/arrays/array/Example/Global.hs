{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/array.h>"
  , "/* test_arraysarray_Example_get_arr0 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_21338ece6a009179 (void))[3]"
  , "{"
  , "  return &arr0;"
  , "}"
  , "/* test_arraysarray_Example_get_arr1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a058c9f90a6e675c (void))[3]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* test_arraysarray_Example_get_arr2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_c57aab7f71d2cb0b (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* test_arraysarray_Example_get_arr3 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e2b7478ee9c12746 (void))[3]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* test_arraysarray_Example_get_arr6 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5f4d28170d6fa0d1 (void))[1]"
  , "{"
  , "  return &arr6;"
  , "}"
  , "/* test_arraysarray_Example_get_arr7 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3b952c95c3eb17f2 (void))[]"
  , "{"
  , "  return &arr7;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2d21b7cb9ac33ea8 (void))[3]"
  , "{"
  , "  return &arr_1;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_2 */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_df706ce2499e5073 (void)"
  , "{"
  , "  return &arr_2;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_3 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_26bcbe886b94adfb (void))[]"
  , "{"
  , "  return &arr_3;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_4 */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_e4cef264a5cc1887 (void)"
  , "{"
  , "  return &arr_4;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_5 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a733c5b896a8ae4e (void))[4][3]"
  , "{"
  , "  return &arr_5;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_6 */"
  , "__attribute__ ((const))"
  , "matrix *hs_bindgen_525a326f0badae7c (void)"
  , "{"
  , "  return &arr_6;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_7 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b5e93d2d90ffc861 (void))[][3]"
  , "{"
  , "  return &arr_7;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_8 */"
  , "__attribute__ ((const))"
  , "tripletlist *hs_bindgen_eb98aff4ee1d31d0 (void)"
  , "{"
  , "  return &arr_8;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_1_const */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_25089a5dd72254bc (void))[3]"
  , "{"
  , "  return &arr_1_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_2_const */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_a55a5562a096ae11 (void)"
  , "{"
  , "  return &arr_2_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_3_const */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_fad31baeec544a6d (void))[]"
  , "{"
  , "  return &arr_3_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_4_const */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_890c46fc872e1d7d (void)"
  , "{"
  , "  return &arr_4_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_5_const */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_088c9ea4e73d1cfe (void))[4][3]"
  , "{"
  , "  return &arr_5_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_6_const */"
  , "__attribute__ ((const))"
  , "matrix const *hs_bindgen_bf69f287b3bf9fa4 (void)"
  , "{"
  , "  return &arr_6_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_7_const */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_2ca44942791eacc4 (void))[][3]"
  , "{"
  , "  return &arr_7_const;"
  , "}"
  , "/* test_arraysarray_Example_get_arr_8_const */"
  , "__attribute__ ((const))"
  , "tripletlist const *hs_bindgen_3e95fdf3b641c349 (void)"
  , "{"
  , "  return &arr_8_const;"
  , "}"
  ]))

-- __unique:__ @test_arraysarray_Example_get_arr0@
foreign import ccall unsafe "hs_bindgen_21338ece6a009179" hs_bindgen_21338ece6a009179_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr0@
hs_bindgen_21338ece6a009179 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_21338ece6a009179 =
  RIP.fromFFIType hs_bindgen_21338ece6a009179_base

{-# NOINLINE arr0 #-}
{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @arrays\/array.h 11:5@

__exported by:__ @arrays\/array.h@
-}
arr0 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr0 =
  RIP.unsafePerformIO hs_bindgen_21338ece6a009179

-- __unique:__ @test_arraysarray_Example_get_arr1@
foreign import ccall unsafe "hs_bindgen_a058c9f90a6e675c" hs_bindgen_a058c9f90a6e675c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr1@
hs_bindgen_a058c9f90a6e675c :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_a058c9f90a6e675c =
  RIP.fromFFIType hs_bindgen_a058c9f90a6e675c_base

{-# NOINLINE arr1 #-}
{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @arrays\/array.h 14:5@

__exported by:__ @arrays\/array.h@
-}
arr1 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr1 =
  RIP.unsafePerformIO hs_bindgen_a058c9f90a6e675c

-- __unique:__ @test_arraysarray_Example_get_arr2@
foreign import ccall unsafe "hs_bindgen_c57aab7f71d2cb0b" hs_bindgen_c57aab7f71d2cb0b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr2@
hs_bindgen_c57aab7f71d2cb0b :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_c57aab7f71d2cb0b =
  RIP.fromFFIType hs_bindgen_c57aab7f71d2cb0b_base

{-# NOINLINE arr2 #-}
{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @arrays\/array.h 17:12@

__exported by:__ @arrays\/array.h@
-}
arr2 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr2 =
  RIP.unsafePerformIO hs_bindgen_c57aab7f71d2cb0b

-- __unique:__ @test_arraysarray_Example_get_arr3@
foreign import ccall unsafe "hs_bindgen_e2b7478ee9c12746" hs_bindgen_e2b7478ee9c12746_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr3@
hs_bindgen_e2b7478ee9c12746 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_e2b7478ee9c12746 =
  RIP.fromFFIType hs_bindgen_e2b7478ee9c12746_base

{-# NOINLINE arr3 #-}
{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @arrays\/array.h 20:12@

__exported by:__ @arrays\/array.h@
-}
arr3 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr3 =
  RIP.unsafePerformIO hs_bindgen_e2b7478ee9c12746

-- __unique:__ @test_arraysarray_Example_get_arr6@
foreign import ccall unsafe "hs_bindgen_5f4d28170d6fa0d1" hs_bindgen_5f4d28170d6fa0d1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr6@
hs_bindgen_5f4d28170d6fa0d1 :: IO (RIP.Ptr ((CA.ConstantArray 1) RIP.CInt))
hs_bindgen_5f4d28170d6fa0d1 =
  RIP.fromFFIType hs_bindgen_5f4d28170d6fa0d1_base

{-# NOINLINE arr6 #-}
{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @arrays\/array.h 29:5@

__exported by:__ @arrays\/array.h@
-}
arr6 :: RIP.Ptr ((CA.ConstantArray 1) RIP.CInt)
arr6 =
  RIP.unsafePerformIO hs_bindgen_5f4d28170d6fa0d1

-- __unique:__ @test_arraysarray_Example_get_arr7@
foreign import ccall unsafe "hs_bindgen_3b952c95c3eb17f2" hs_bindgen_3b952c95c3eb17f2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr7@
hs_bindgen_3b952c95c3eb17f2 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
hs_bindgen_3b952c95c3eb17f2 =
  RIP.fromFFIType hs_bindgen_3b952c95c3eb17f2_base

{-# NOINLINE arr7 #-}
{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @arrays\/array.h 32:12@

__exported by:__ @arrays\/array.h@
-}
arr7 :: RIP.Ptr (IA.IncompleteArray RIP.CInt)
arr7 =
  RIP.unsafePerformIO hs_bindgen_3b952c95c3eb17f2

-- __unique:__ @test_arraysarray_Example_get_arr_1@
foreign import ccall unsafe "hs_bindgen_2d21b7cb9ac33ea8" hs_bindgen_2d21b7cb9ac33ea8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_1@
hs_bindgen_2d21b7cb9ac33ea8 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_2d21b7cb9ac33ea8 =
  RIP.fromFFIType hs_bindgen_2d21b7cb9ac33ea8_base

{-# NOINLINE arr_1 #-}
{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @arrays\/array.h 62:12@

__exported by:__ @arrays\/array.h@
-}
arr_1 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
arr_1 =
  RIP.unsafePerformIO hs_bindgen_2d21b7cb9ac33ea8

-- __unique:__ @test_arraysarray_Example_get_arr_2@
foreign import ccall unsafe "hs_bindgen_df706ce2499e5073" hs_bindgen_df706ce2499e5073_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_2@
hs_bindgen_df706ce2499e5073 :: IO (RIP.Ptr Triplet)
hs_bindgen_df706ce2499e5073 =
  RIP.fromFFIType hs_bindgen_df706ce2499e5073_base

{-# NOINLINE arr_2 #-}
{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @arrays\/array.h 65:16@

__exported by:__ @arrays\/array.h@
-}
arr_2 :: RIP.Ptr Triplet
arr_2 =
  RIP.unsafePerformIO hs_bindgen_df706ce2499e5073

-- __unique:__ @test_arraysarray_Example_get_arr_3@
foreign import ccall unsafe "hs_bindgen_26bcbe886b94adfb" hs_bindgen_26bcbe886b94adfb_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_3@
hs_bindgen_26bcbe886b94adfb :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
hs_bindgen_26bcbe886b94adfb =
  RIP.fromFFIType hs_bindgen_26bcbe886b94adfb_base

{-# NOINLINE arr_3 #-}
{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @arrays\/array.h 68:12@

__exported by:__ @arrays\/array.h@
-}
arr_3 :: RIP.Ptr (IA.IncompleteArray RIP.CInt)
arr_3 =
  RIP.unsafePerformIO hs_bindgen_26bcbe886b94adfb

-- __unique:__ @test_arraysarray_Example_get_arr_4@
foreign import ccall unsafe "hs_bindgen_e4cef264a5cc1887" hs_bindgen_e4cef264a5cc1887_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_4@
hs_bindgen_e4cef264a5cc1887 :: IO (RIP.Ptr List)
hs_bindgen_e4cef264a5cc1887 =
  RIP.fromFFIType hs_bindgen_e4cef264a5cc1887_base

{-# NOINLINE arr_4 #-}
{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @arrays\/array.h 71:13@

__exported by:__ @arrays\/array.h@
-}
arr_4 :: RIP.Ptr List
arr_4 =
  RIP.unsafePerformIO hs_bindgen_e4cef264a5cc1887

-- __unique:__ @test_arraysarray_Example_get_arr_5@
foreign import ccall unsafe "hs_bindgen_a733c5b896a8ae4e" hs_bindgen_a733c5b896a8ae4e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_5@
hs_bindgen_a733c5b896a8ae4e :: IO (RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_a733c5b896a8ae4e =
  RIP.fromFFIType hs_bindgen_a733c5b896a8ae4e_base

{-# NOINLINE arr_5 #-}
{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @arrays\/array.h 74:12@

__exported by:__ @arrays\/array.h@
-}
arr_5 :: RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
arr_5 =
  RIP.unsafePerformIO hs_bindgen_a733c5b896a8ae4e

-- __unique:__ @test_arraysarray_Example_get_arr_6@
foreign import ccall unsafe "hs_bindgen_525a326f0badae7c" hs_bindgen_525a326f0badae7c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_6@
hs_bindgen_525a326f0badae7c :: IO (RIP.Ptr Matrix)
hs_bindgen_525a326f0badae7c =
  RIP.fromFFIType hs_bindgen_525a326f0badae7c_base

{-# NOINLINE arr_6 #-}
{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @arrays\/array.h 77:15@

__exported by:__ @arrays\/array.h@
-}
arr_6 :: RIP.Ptr Matrix
arr_6 =
  RIP.unsafePerformIO hs_bindgen_525a326f0badae7c

-- __unique:__ @test_arraysarray_Example_get_arr_7@
foreign import ccall unsafe "hs_bindgen_b5e93d2d90ffc861" hs_bindgen_b5e93d2d90ffc861_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_7@
hs_bindgen_b5e93d2d90ffc861 :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_b5e93d2d90ffc861 =
  RIP.fromFFIType hs_bindgen_b5e93d2d90ffc861_base

{-# NOINLINE arr_7 #-}
{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @arrays\/array.h 80:12@

__exported by:__ @arrays\/array.h@
-}
arr_7 :: RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
arr_7 =
  RIP.unsafePerformIO hs_bindgen_b5e93d2d90ffc861

-- __unique:__ @test_arraysarray_Example_get_arr_8@
foreign import ccall unsafe "hs_bindgen_eb98aff4ee1d31d0" hs_bindgen_eb98aff4ee1d31d0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_8@
hs_bindgen_eb98aff4ee1d31d0 :: IO (RIP.Ptr Tripletlist)
hs_bindgen_eb98aff4ee1d31d0 =
  RIP.fromFFIType hs_bindgen_eb98aff4ee1d31d0_base

{-# NOINLINE arr_8 #-}
{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @arrays\/array.h 83:20@

__exported by:__ @arrays\/array.h@
-}
arr_8 :: RIP.Ptr Tripletlist
arr_8 =
  RIP.unsafePerformIO hs_bindgen_eb98aff4ee1d31d0

-- __unique:__ @test_arraysarray_Example_get_arr_1_const@
foreign import ccall unsafe "hs_bindgen_25089a5dd72254bc" hs_bindgen_25089a5dd72254bc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_1_const@
hs_bindgen_25089a5dd72254bc :: IO (PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_25089a5dd72254bc =
  RIP.fromFFIType hs_bindgen_25089a5dd72254bc_base

{-# NOINLINE hs_bindgen_a182a929fe06593d #-}
{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @arrays\/array.h 90:18@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_arr_1_const@
-}
hs_bindgen_a182a929fe06593d :: PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
hs_bindgen_a182a929fe06593d =
  RIP.unsafePerformIO hs_bindgen_25089a5dd72254bc

{-# NOINLINE arr_1_const #-}
arr_1_const :: (CA.ConstantArray 3) RIP.CInt
arr_1_const =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_a182a929fe06593d)

-- __unique:__ @test_arraysarray_Example_get_arr_2_const@
foreign import ccall unsafe "hs_bindgen_a55a5562a096ae11" hs_bindgen_a55a5562a096ae11_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_2_const@
hs_bindgen_a55a5562a096ae11 :: IO (PtrConst.PtrConst Triplet)
hs_bindgen_a55a5562a096ae11 =
  RIP.fromFFIType hs_bindgen_a55a5562a096ae11_base

{-# NOINLINE hs_bindgen_40c72338f5e41246 #-}
{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @arrays\/array.h 93:22@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_arr_2_const@
-}
hs_bindgen_40c72338f5e41246 :: PtrConst.PtrConst Triplet
hs_bindgen_40c72338f5e41246 =
  RIP.unsafePerformIO hs_bindgen_a55a5562a096ae11

{-# NOINLINE arr_2_const #-}
arr_2_const :: Triplet
arr_2_const =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_40c72338f5e41246)

-- __unique:__ @test_arraysarray_Example_get_arr_3_const@
foreign import ccall unsafe "hs_bindgen_fad31baeec544a6d" hs_bindgen_fad31baeec544a6d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_3_const@
hs_bindgen_fad31baeec544a6d :: IO (PtrConst.PtrConst (IA.IncompleteArray RIP.CInt))
hs_bindgen_fad31baeec544a6d =
  RIP.fromFFIType hs_bindgen_fad31baeec544a6d_base

{-# NOINLINE arr_3_const #-}
{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @arrays\/array.h 96:18@

__exported by:__ @arrays\/array.h@
-}
arr_3_const :: PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
arr_3_const =
  RIP.unsafePerformIO hs_bindgen_fad31baeec544a6d

-- __unique:__ @test_arraysarray_Example_get_arr_4_const@
foreign import ccall unsafe "hs_bindgen_890c46fc872e1d7d" hs_bindgen_890c46fc872e1d7d_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_4_const@
hs_bindgen_890c46fc872e1d7d :: IO (PtrConst.PtrConst List)
hs_bindgen_890c46fc872e1d7d =
  RIP.fromFFIType hs_bindgen_890c46fc872e1d7d_base

{-# NOINLINE arr_4_const #-}
{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @arrays\/array.h 99:19@

__exported by:__ @arrays\/array.h@
-}
arr_4_const :: PtrConst.PtrConst List
arr_4_const =
  RIP.unsafePerformIO hs_bindgen_890c46fc872e1d7d

-- __unique:__ @test_arraysarray_Example_get_arr_5_const@
foreign import ccall unsafe "hs_bindgen_088c9ea4e73d1cfe" hs_bindgen_088c9ea4e73d1cfe_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_5_const@
hs_bindgen_088c9ea4e73d1cfe :: IO (PtrConst.PtrConst ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_088c9ea4e73d1cfe =
  RIP.fromFFIType hs_bindgen_088c9ea4e73d1cfe_base

{-# NOINLINE hs_bindgen_be82907b3025ae62 #-}
{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @arrays\/array.h 102:18@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_arr_5_const@
-}
hs_bindgen_be82907b3025ae62 :: PtrConst.PtrConst ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_be82907b3025ae62 =
  RIP.unsafePerformIO hs_bindgen_088c9ea4e73d1cfe

{-# NOINLINE arr_5_const #-}
arr_5_const :: (CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)
arr_5_const =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_be82907b3025ae62)

-- __unique:__ @test_arraysarray_Example_get_arr_6_const@
foreign import ccall unsafe "hs_bindgen_bf69f287b3bf9fa4" hs_bindgen_bf69f287b3bf9fa4_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_6_const@
hs_bindgen_bf69f287b3bf9fa4 :: IO (PtrConst.PtrConst Matrix)
hs_bindgen_bf69f287b3bf9fa4 =
  RIP.fromFFIType hs_bindgen_bf69f287b3bf9fa4_base

{-# NOINLINE hs_bindgen_50556ee70fdb04aa #-}
{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @arrays\/array.h 105:21@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_arr_6_const@
-}
hs_bindgen_50556ee70fdb04aa :: PtrConst.PtrConst Matrix
hs_bindgen_50556ee70fdb04aa =
  RIP.unsafePerformIO hs_bindgen_bf69f287b3bf9fa4

{-# NOINLINE arr_6_const #-}
arr_6_const :: Matrix
arr_6_const =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_50556ee70fdb04aa)

-- __unique:__ @test_arraysarray_Example_get_arr_7_const@
foreign import ccall unsafe "hs_bindgen_2ca44942791eacc4" hs_bindgen_2ca44942791eacc4_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_7_const@
hs_bindgen_2ca44942791eacc4 :: IO (PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_2ca44942791eacc4 =
  RIP.fromFFIType hs_bindgen_2ca44942791eacc4_base

{-# NOINLINE arr_7_const #-}
{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @arrays\/array.h 108:18@

__exported by:__ @arrays\/array.h@
-}
arr_7_const :: PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
arr_7_const =
  RIP.unsafePerformIO hs_bindgen_2ca44942791eacc4

-- __unique:__ @test_arraysarray_Example_get_arr_8_const@
foreign import ccall unsafe "hs_bindgen_3e95fdf3b641c349" hs_bindgen_3e95fdf3b641c349_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_get_arr_8_const@
hs_bindgen_3e95fdf3b641c349 :: IO (PtrConst.PtrConst Tripletlist)
hs_bindgen_3e95fdf3b641c349 =
  RIP.fromFFIType hs_bindgen_3e95fdf3b641c349_base

{-# NOINLINE arr_8_const #-}
{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @arrays\/array.h 111:26@

__exported by:__ @arrays\/array.h@
-}
arr_8_const :: PtrConst.PtrConst Tripletlist
arr_8_const =
  RIP.unsafePerformIO hs_bindgen_3e95fdf3b641c349
