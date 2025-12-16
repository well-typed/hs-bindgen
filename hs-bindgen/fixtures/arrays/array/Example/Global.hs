{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
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

-- | __unique:__ @test_arraysarray_Example_get_arr0@
foreign import ccall unsafe "hs_bindgen_21338ece6a009179" hs_bindgen_21338ece6a009179 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0 #-}

{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @arrays\/array.h:11:5@

__exported by:__ @arrays\/array.h@
-}
arr0 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_21338ece6a009179

-- | __unique:__ @test_arraysarray_Example_get_arr1@
foreign import ccall unsafe "hs_bindgen_a058c9f90a6e675c" hs_bindgen_a058c9f90a6e675c ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1 #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @arrays\/array.h:14:5@

__exported by:__ @arrays\/array.h@
-}
arr1 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a058c9f90a6e675c

-- | __unique:__ @test_arraysarray_Example_get_arr2@
foreign import ccall unsafe "hs_bindgen_c57aab7f71d2cb0b" hs_bindgen_c57aab7f71d2cb0b ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2 #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @arrays\/array.h:17:12@

__exported by:__ @arrays\/array.h@
-}
arr2 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c57aab7f71d2cb0b

-- | __unique:__ @test_arraysarray_Example_get_arr3@
foreign import ccall unsafe "hs_bindgen_e2b7478ee9c12746" hs_bindgen_e2b7478ee9c12746 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3 #-}

{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @arrays\/array.h:20:12@

__exported by:__ @arrays\/array.h@
-}
arr3 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e2b7478ee9c12746

-- | __unique:__ @test_arraysarray_Example_get_arr6@
foreign import ccall unsafe "hs_bindgen_5f4d28170d6fa0d1" hs_bindgen_5f4d28170d6fa0d1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6 #-}

{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @arrays\/array.h:29:5@

__exported by:__ @arrays\/array.h@
-}
arr6 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5f4d28170d6fa0d1

-- | __unique:__ @test_arraysarray_Example_get_arr7@
foreign import ccall unsafe "hs_bindgen_3b952c95c3eb17f2" hs_bindgen_3b952c95c3eb17f2 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7 #-}

{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @arrays\/array.h:32:12@

__exported by:__ @arrays\/array.h@
-}
arr7 :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3b952c95c3eb17f2

-- | __unique:__ @test_arraysarray_Example_get_arr_1@
foreign import ccall unsafe "hs_bindgen_2d21b7cb9ac33ea8" hs_bindgen_2d21b7cb9ac33ea8 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1 #-}

{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @arrays\/array.h:62:12@

__exported by:__ @arrays\/array.h@
-}
arr_1 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d21b7cb9ac33ea8

-- | __unique:__ @test_arraysarray_Example_get_arr_2@
foreign import ccall unsafe "hs_bindgen_df706ce2499e5073" hs_bindgen_df706ce2499e5073 ::
     IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2 #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @arrays\/array.h:65:16@

__exported by:__ @arrays\/array.h@
-}
arr_2 :: Ptr.Ptr Triplet
arr_2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_df706ce2499e5073

-- | __unique:__ @test_arraysarray_Example_get_arr_3@
foreign import ccall unsafe "hs_bindgen_26bcbe886b94adfb" hs_bindgen_26bcbe886b94adfb ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3 #-}

{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @arrays\/array.h:68:12@

__exported by:__ @arrays\/array.h@
-}
arr_3 :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_26bcbe886b94adfb

-- | __unique:__ @test_arraysarray_Example_get_arr_4@
foreign import ccall unsafe "hs_bindgen_e4cef264a5cc1887" hs_bindgen_e4cef264a5cc1887 ::
     IO (Ptr.Ptr List)

{-# NOINLINE arr_4 #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @arrays\/array.h:71:13@

__exported by:__ @arrays\/array.h@
-}
arr_4 :: Ptr.Ptr List
arr_4 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e4cef264a5cc1887

-- | __unique:__ @test_arraysarray_Example_get_arr_5@
foreign import ccall unsafe "hs_bindgen_a733c5b896a8ae4e" hs_bindgen_a733c5b896a8ae4e ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5 #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @arrays\/array.h:74:12@

__exported by:__ @arrays\/array.h@
-}
arr_5 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a733c5b896a8ae4e

-- | __unique:__ @test_arraysarray_Example_get_arr_6@
foreign import ccall unsafe "hs_bindgen_525a326f0badae7c" hs_bindgen_525a326f0badae7c ::
     IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6 #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @arrays\/array.h:77:15@

__exported by:__ @arrays\/array.h@
-}
arr_6 :: Ptr.Ptr Matrix
arr_6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_525a326f0badae7c

-- | __unique:__ @test_arraysarray_Example_get_arr_7@
foreign import ccall unsafe "hs_bindgen_b5e93d2d90ffc861" hs_bindgen_b5e93d2d90ffc861 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7 #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @arrays\/array.h:80:12@

__exported by:__ @arrays\/array.h@
-}
arr_7 :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b5e93d2d90ffc861

-- | __unique:__ @test_arraysarray_Example_get_arr_8@
foreign import ccall unsafe "hs_bindgen_eb98aff4ee1d31d0" hs_bindgen_eb98aff4ee1d31d0 ::
     IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8 #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @arrays\/array.h:83:20@

__exported by:__ @arrays\/array.h@
-}
arr_8 :: Ptr.Ptr Tripletlist
arr_8 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_eb98aff4ee1d31d0

-- | __unique:__ @test_arraysarray_Example_get_arr_1_const@
foreign import ccall unsafe "hs_bindgen_25089a5dd72254bc" hs_bindgen_25089a5dd72254bc ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE hs_bindgen_a182a929fe06593d #-}

{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @arrays\/array.h:90:18@

__exported by:__ @arrays\/array.h@
-}
hs_bindgen_a182a929fe06593d :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
hs_bindgen_a182a929fe06593d =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_25089a5dd72254bc

{-# NOINLINE arr_1_const #-}

arr_1_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
arr_1_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_a182a929fe06593d))

-- | __unique:__ @test_arraysarray_Example_get_arr_2_const@
foreign import ccall unsafe "hs_bindgen_a55a5562a096ae11" hs_bindgen_a55a5562a096ae11 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Triplet)

{-# NOINLINE hs_bindgen_40c72338f5e41246 #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @arrays\/array.h:93:22@

__exported by:__ @arrays\/array.h@
-}
hs_bindgen_40c72338f5e41246 :: HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
hs_bindgen_40c72338f5e41246 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a55a5562a096ae11

{-# NOINLINE arr_2_const #-}

arr_2_const :: Triplet
arr_2_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_40c72338f5e41246))

-- | __unique:__ @test_arraysarray_Example_get_arr_3_const@
foreign import ccall unsafe "hs_bindgen_fad31baeec544a6d" hs_bindgen_fad31baeec544a6d ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_const #-}

{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @arrays\/array.h:96:18@

__exported by:__ @arrays\/array.h@
-}
arr_3_const :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fad31baeec544a6d

-- | __unique:__ @test_arraysarray_Example_get_arr_4_const@
foreign import ccall unsafe "hs_bindgen_890c46fc872e1d7d" hs_bindgen_890c46fc872e1d7d ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr List)

{-# NOINLINE arr_4_const #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @arrays\/array.h:99:19@

__exported by:__ @arrays\/array.h@
-}
arr_4_const :: HsBindgen.Runtime.ConstPtr.ConstPtr List
arr_4_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_890c46fc872e1d7d

-- | __unique:__ @test_arraysarray_Example_get_arr_5_const@
foreign import ccall unsafe "hs_bindgen_088c9ea4e73d1cfe" hs_bindgen_088c9ea4e73d1cfe ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE hs_bindgen_be82907b3025ae62 #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @arrays\/array.h:102:18@

__exported by:__ @arrays\/array.h@
-}
hs_bindgen_be82907b3025ae62 :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_be82907b3025ae62 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_088c9ea4e73d1cfe

{-# NOINLINE arr_5_const #-}

arr_5_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_5_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_be82907b3025ae62))

-- | __unique:__ @test_arraysarray_Example_get_arr_6_const@
foreign import ccall unsafe "hs_bindgen_bf69f287b3bf9fa4" hs_bindgen_bf69f287b3bf9fa4 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Matrix)

{-# NOINLINE hs_bindgen_50556ee70fdb04aa #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @arrays\/array.h:105:21@

__exported by:__ @arrays\/array.h@
-}
hs_bindgen_50556ee70fdb04aa :: HsBindgen.Runtime.ConstPtr.ConstPtr Matrix
hs_bindgen_50556ee70fdb04aa =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bf69f287b3bf9fa4

{-# NOINLINE arr_6_const #-}

arr_6_const :: Matrix
arr_6_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr hs_bindgen_50556ee70fdb04aa))

-- | __unique:__ @test_arraysarray_Example_get_arr_7_const@
foreign import ccall unsafe "hs_bindgen_2ca44942791eacc4" hs_bindgen_2ca44942791eacc4 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_const #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @arrays\/array.h:108:18@

__exported by:__ @arrays\/array.h@
-}
arr_7_const :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2ca44942791eacc4

-- | __unique:__ @test_arraysarray_Example_get_arr_8_const@
foreign import ccall unsafe "hs_bindgen_3e95fdf3b641c349" hs_bindgen_3e95fdf3b641c349 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Tripletlist)

{-# NOINLINE arr_8_const #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @arrays\/array.h:111:26@

__exported by:__ @arrays\/array.h@
-}
arr_8_const :: HsBindgen.Runtime.ConstPtr.ConstPtr Tripletlist
arr_8_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3e95fdf3b641c349
