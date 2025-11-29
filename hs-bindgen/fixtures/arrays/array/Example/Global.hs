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
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "/* ExampleNothingget_arr0_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_a6413f4d2092265d (void))[3]"
  , "{"
  , "  return &arr0;"
  , "}"
  , "/* ExampleNothingget_arr1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_1693226264ba4aeb (void))[3]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* ExampleNothingget_arr2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_dafcf99a73b93389 (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* ExampleNothingget_arr3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_ca1016acc3449dee (void))[3]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* ExampleNothingget_arr6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_1a8c921160bc99a6 (void))[1]"
  , "{"
  , "  return &arr6;"
  , "}"
  , "/* ExampleNothingget_arr7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_17cf970243739b65 (void))[]"
  , "{"
  , "  return &arr7;"
  , "}"
  , "/* ExampleNothingget_arr_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_85bc33b188037456 (void))[3]"
  , "{"
  , "  return &arr_1;"
  , "}"
  , "/* ExampleNothingget_arr_2_ptr */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_test_arraysarray_87c784150cd3ff65 (void)"
  , "{"
  , "  return &arr_2;"
  , "}"
  , "/* ExampleNothingget_arr_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_e7b0de7633a7a62a (void))[]"
  , "{"
  , "  return &arr_3;"
  , "}"
  , "/* ExampleNothingget_arr_4_ptr */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_test_arraysarray_8fb64bc6c2bd4c73 (void)"
  , "{"
  , "  return &arr_4;"
  , "}"
  , "/* ExampleNothingget_arr_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_7348a94e6adce96e (void))[4][3]"
  , "{"
  , "  return &arr_5;"
  , "}"
  , "/* ExampleNothingget_arr_6_ptr */"
  , "__attribute__ ((const))"
  , "matrix *hs_bindgen_test_arraysarray_1308613140bb4b80 (void)"
  , "{"
  , "  return &arr_6;"
  , "}"
  , "/* ExampleNothingget_arr_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_a060984b378ed676 (void))[][3]"
  , "{"
  , "  return &arr_7;"
  , "}"
  , "/* ExampleNothingget_arr_8_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *hs_bindgen_test_arraysarray_d82706abb6d8ea04 (void)"
  , "{"
  , "  return &arr_8;"
  , "}"
  , "/* ExampleNothingget_arr_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_7376d172f5729493 (void))[3]"
  , "{"
  , "  return &arr_1_const;"
  , "}"
  , "/* ExampleNothingget_arr_2_const_ptr */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_test_arraysarray_f03586aa57dfce29 (void)"
  , "{"
  , "  return &arr_2_const;"
  , "}"
  , "/* ExampleNothingget_arr_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_54ffd4ffcd2dad61 (void))[]"
  , "{"
  , "  return &arr_3_const;"
  , "}"
  , "/* ExampleNothingget_arr_4_const_ptr */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_test_arraysarray_8896c2ff5b9ce9c9 (void)"
  , "{"
  , "  return &arr_4_const;"
  , "}"
  , "/* ExampleNothingget_arr_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_46b406e096f6c9c1 (void))[4][3]"
  , "{"
  , "  return &arr_5_const;"
  , "}"
  , "/* ExampleNothingget_arr_6_const_ptr */"
  , "__attribute__ ((const))"
  , "matrix const *hs_bindgen_test_arraysarray_ceb7f2027865ce12 (void)"
  , "{"
  , "  return &arr_6_const;"
  , "}"
  , "/* ExampleNothingget_arr_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_2b565b2b97acdcb7 (void))[][3]"
  , "{"
  , "  return &arr_7_const;"
  , "}"
  , "/* ExampleNothingget_arr_8_const_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist const *hs_bindgen_test_arraysarray_03e2d9c4ef2ae993 (void)"
  , "{"
  , "  return &arr_8_const;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_arr0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a6413f4d2092265d" hs_bindgen_test_arraysarray_a6413f4d2092265d ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0_ptr #-}

{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @arrays\/array.h:11:5@

__exported by:__ @arrays\/array.h@
-}
arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a6413f4d2092265d

{-| __unique:__ @ExampleNothingget_arr1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_1693226264ba4aeb" hs_bindgen_test_arraysarray_1693226264ba4aeb ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @arrays\/array.h:14:5@

__exported by:__ @arrays\/array.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_1693226264ba4aeb

{-| __unique:__ @ExampleNothingget_arr2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_dafcf99a73b93389" hs_bindgen_test_arraysarray_dafcf99a73b93389 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @arrays\/array.h:17:12@

__exported by:__ @arrays\/array.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_dafcf99a73b93389

{-| __unique:__ @ExampleNothingget_arr3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_ca1016acc3449dee" hs_bindgen_test_arraysarray_ca1016acc3449dee ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @arrays\/array.h:20:12@

__exported by:__ @arrays\/array.h@
-}
arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_ca1016acc3449dee

{-| __unique:__ @ExampleNothingget_arr6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_1a8c921160bc99a6" hs_bindgen_test_arraysarray_1a8c921160bc99a6 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6_ptr #-}

{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @arrays\/array.h:29:5@

__exported by:__ @arrays\/array.h@
-}
arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_1a8c921160bc99a6

{-| __unique:__ @ExampleNothingget_arr7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_17cf970243739b65" hs_bindgen_test_arraysarray_17cf970243739b65 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @arrays\/array.h:32:12@

__exported by:__ @arrays\/array.h@
-}
arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_17cf970243739b65

{-| __unique:__ @ExampleNothingget_arr_1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_85bc33b188037456" hs_bindgen_test_arraysarray_85bc33b188037456 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @arrays\/array.h:62:12@

__exported by:__ @arrays\/array.h@
-}
arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_85bc33b188037456

{-| __unique:__ @ExampleNothingget_arr_2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_87c784150cd3ff65" hs_bindgen_test_arraysarray_87c784150cd3ff65 ::
     IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @arrays\/array.h:65:16@

__exported by:__ @arrays\/array.h@
-}
arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_87c784150cd3ff65

{-| __unique:__ @ExampleNothingget_arr_3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_e7b0de7633a7a62a" hs_bindgen_test_arraysarray_e7b0de7633a7a62a ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @arrays\/array.h:68:12@

__exported by:__ @arrays\/array.h@
-}
arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_e7b0de7633a7a62a

{-| __unique:__ @ExampleNothingget_arr_4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_8fb64bc6c2bd4c73" hs_bindgen_test_arraysarray_8fb64bc6c2bd4c73 ::
     IO (Ptr.Ptr List)

{-# NOINLINE arr_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @arrays\/array.h:71:13@

__exported by:__ @arrays\/array.h@
-}
arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_8fb64bc6c2bd4c73

{-| __unique:__ @ExampleNothingget_arr_5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_7348a94e6adce96e" hs_bindgen_test_arraysarray_7348a94e6adce96e ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @arrays\/array.h:74:12@

__exported by:__ @arrays\/array.h@
-}
arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_7348a94e6adce96e

{-| __unique:__ @ExampleNothingget_arr_6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_1308613140bb4b80" hs_bindgen_test_arraysarray_1308613140bb4b80 ::
     IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @arrays\/array.h:77:15@

__exported by:__ @arrays\/array.h@
-}
arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_1308613140bb4b80

{-| __unique:__ @ExampleNothingget_arr_7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a060984b378ed676" hs_bindgen_test_arraysarray_a060984b378ed676 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @arrays\/array.h:80:12@

__exported by:__ @arrays\/array.h@
-}
arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a060984b378ed676

{-| __unique:__ @ExampleNothingget_arr_8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d82706abb6d8ea04" hs_bindgen_test_arraysarray_d82706abb6d8ea04 ::
     IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @arrays\/array.h:83:20@

__exported by:__ @arrays\/array.h@
-}
arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d82706abb6d8ea04

{-| __unique:__ @ExampleNothingget_arr_1_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_7376d172f5729493" hs_bindgen_test_arraysarray_7376d172f5729493 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @arrays\/array.h:90:18@

__exported by:__ @arrays\/array.h@
-}
arr_1_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_7376d172f5729493

{-# NOINLINE arr_1_const #-}

arr_1_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
arr_1_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_1_const_ptr)

{-| __unique:__ @ExampleNothingget_arr_2_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_f03586aa57dfce29" hs_bindgen_test_arraysarray_f03586aa57dfce29 ::
     IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @arrays\/array.h:93:22@

__exported by:__ @arrays\/array.h@
-}
arr_2_const_ptr :: Ptr.Ptr Triplet
arr_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_f03586aa57dfce29

{-# NOINLINE arr_2_const #-}

arr_2_const :: Triplet
arr_2_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_2_const_ptr)

{-| __unique:__ @ExampleNothingget_arr_3_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_54ffd4ffcd2dad61" hs_bindgen_test_arraysarray_54ffd4ffcd2dad61 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @arrays\/array.h:96:18@

__exported by:__ @arrays\/array.h@
-}
arr_3_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_54ffd4ffcd2dad61

{-| __unique:__ @ExampleNothingget_arr_4_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_8896c2ff5b9ce9c9" hs_bindgen_test_arraysarray_8896c2ff5b9ce9c9 ::
     IO (Ptr.Ptr List)

{-# NOINLINE arr_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @arrays\/array.h:99:19@

__exported by:__ @arrays\/array.h@
-}
arr_4_const_ptr :: Ptr.Ptr List
arr_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_8896c2ff5b9ce9c9

{-| __unique:__ @ExampleNothingget_arr_5_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_46b406e096f6c9c1" hs_bindgen_test_arraysarray_46b406e096f6c9c1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @arrays\/array.h:102:18@

__exported by:__ @arrays\/array.h@
-}
arr_5_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_46b406e096f6c9c1

{-# NOINLINE arr_5_const #-}

arr_5_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_5_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_5_const_ptr)

{-| __unique:__ @ExampleNothingget_arr_6_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_ceb7f2027865ce12" hs_bindgen_test_arraysarray_ceb7f2027865ce12 ::
     IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @arrays\/array.h:105:21@

__exported by:__ @arrays\/array.h@
-}
arr_6_const_ptr :: Ptr.Ptr Matrix
arr_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_ceb7f2027865ce12

{-# NOINLINE arr_6_const #-}

arr_6_const :: Matrix
arr_6_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_6_const_ptr)

{-| __unique:__ @ExampleNothingget_arr_7_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_2b565b2b97acdcb7" hs_bindgen_test_arraysarray_2b565b2b97acdcb7 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @arrays\/array.h:108:18@

__exported by:__ @arrays\/array.h@
-}
arr_7_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_2b565b2b97acdcb7

{-| __unique:__ @ExampleNothingget_arr_8_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_03e2d9c4ef2ae993" hs_bindgen_test_arraysarray_03e2d9c4ef2ae993 ::
     IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @arrays\/array.h:111:26@

__exported by:__ @arrays\/array.h@
-}
arr_8_const_ptr :: Ptr.Ptr Tripletlist
arr_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_03e2d9c4ef2ae993
