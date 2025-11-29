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
  , "/* Example_get_arr0_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_c858272a197e766a (void))[3]"
  , "{"
  , "  return &arr0;"
  , "}"
  , "/* Example_get_arr1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_51f7c8357919d458 (void))[3]"
  , "{"
  , "  return &arr1;"
  , "}"
  , "/* Example_get_arr2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_a4d2a767c8544a15 (void))[3]"
  , "{"
  , "  return &arr2;"
  , "}"
  , "/* Example_get_arr3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_1c7096cbfd263693 (void))[3]"
  , "{"
  , "  return &arr3;"
  , "}"
  , "/* Example_get_arr6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_d37a3579003752e0 (void))[1]"
  , "{"
  , "  return &arr6;"
  , "}"
  , "/* Example_get_arr7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_2c0ae51fd14affdd (void))[]"
  , "{"
  , "  return &arr7;"
  , "}"
  , "/* Example_get_arr_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_231950dc7fbd026f (void))[3]"
  , "{"
  , "  return &arr_1;"
  , "}"
  , "/* Example_get_arr_2_ptr */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_test_arraysarray_0ac05d73d8459939 (void)"
  , "{"
  , "  return &arr_2;"
  , "}"
  , "/* Example_get_arr_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_e65f5d175373d7d0 (void))[]"
  , "{"
  , "  return &arr_3;"
  , "}"
  , "/* Example_get_arr_4_ptr */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_test_arraysarray_f5ff472666eacc51 (void)"
  , "{"
  , "  return &arr_4;"
  , "}"
  , "/* Example_get_arr_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_ee4edbac53dee1f5 (void))[4][3]"
  , "{"
  , "  return &arr_5;"
  , "}"
  , "/* Example_get_arr_6_ptr */"
  , "__attribute__ ((const))"
  , "matrix *hs_bindgen_test_arraysarray_9b28f4801607118b (void)"
  , "{"
  , "  return &arr_6;"
  , "}"
  , "/* Example_get_arr_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_d32b62c9cd2ceab5 (void))[][3]"
  , "{"
  , "  return &arr_7;"
  , "}"
  , "/* Example_get_arr_8_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *hs_bindgen_test_arraysarray_a300d3453acf6bf3 (void)"
  , "{"
  , "  return &arr_8;"
  , "}"
  , "/* Example_get_arr_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_a4aaceddda1cb5c1 (void))[3]"
  , "{"
  , "  return &arr_1_const;"
  , "}"
  , "/* Example_get_arr_2_const_ptr */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_test_arraysarray_b7b07f85effc1faa (void)"
  , "{"
  , "  return &arr_2_const;"
  , "}"
  , "/* Example_get_arr_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_6110ddf1070fea5d (void))[]"
  , "{"
  , "  return &arr_3_const;"
  , "}"
  , "/* Example_get_arr_4_const_ptr */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_test_arraysarray_c7988096e618383a (void)"
  , "{"
  , "  return &arr_4_const;"
  , "}"
  , "/* Example_get_arr_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_ab0670fbdb25bf13 (void))[4][3]"
  , "{"
  , "  return &arr_5_const;"
  , "}"
  , "/* Example_get_arr_6_const_ptr */"
  , "__attribute__ ((const))"
  , "matrix const *hs_bindgen_test_arraysarray_6f0b225d9fb26332 (void)"
  , "{"
  , "  return &arr_6_const;"
  , "}"
  , "/* Example_get_arr_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_arraysarray_6be26ad867a2255f (void))[][3]"
  , "{"
  , "  return &arr_7_const;"
  , "}"
  , "/* Example_get_arr_8_const_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist const *hs_bindgen_test_arraysarray_d2885ea24529e66c (void)"
  , "{"
  , "  return &arr_8_const;"
  , "}"
  ]))

{-| __unique:__ @Example_get_arr0_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_c858272a197e766a" hs_bindgen_test_arraysarray_c858272a197e766a ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0_ptr #-}

{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @arrays\/array.h:11:5@

__exported by:__ @arrays\/array.h@
-}
arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_c858272a197e766a

{-| __unique:__ @Example_get_arr1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_51f7c8357919d458" hs_bindgen_test_arraysarray_51f7c8357919d458 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @arrays\/array.h:14:5@

__exported by:__ @arrays\/array.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_51f7c8357919d458

{-| __unique:__ @Example_get_arr2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a4d2a767c8544a15" hs_bindgen_test_arraysarray_a4d2a767c8544a15 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @arrays\/array.h:17:12@

__exported by:__ @arrays\/array.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a4d2a767c8544a15

{-| __unique:__ @Example_get_arr3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_1c7096cbfd263693" hs_bindgen_test_arraysarray_1c7096cbfd263693 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @arrays\/array.h:20:12@

__exported by:__ @arrays\/array.h@
-}
arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_1c7096cbfd263693

{-| __unique:__ @Example_get_arr6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d37a3579003752e0" hs_bindgen_test_arraysarray_d37a3579003752e0 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6_ptr #-}

{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @arrays\/array.h:29:5@

__exported by:__ @arrays\/array.h@
-}
arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d37a3579003752e0

{-| __unique:__ @Example_get_arr7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_2c0ae51fd14affdd" hs_bindgen_test_arraysarray_2c0ae51fd14affdd ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @arrays\/array.h:32:12@

__exported by:__ @arrays\/array.h@
-}
arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_2c0ae51fd14affdd

{-| __unique:__ @Example_get_arr_1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_231950dc7fbd026f" hs_bindgen_test_arraysarray_231950dc7fbd026f ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @arrays\/array.h:62:12@

__exported by:__ @arrays\/array.h@
-}
arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_231950dc7fbd026f

{-| __unique:__ @Example_get_arr_2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_0ac05d73d8459939" hs_bindgen_test_arraysarray_0ac05d73d8459939 ::
     IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @arrays\/array.h:65:16@

__exported by:__ @arrays\/array.h@
-}
arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_0ac05d73d8459939

{-| __unique:__ @Example_get_arr_3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_e65f5d175373d7d0" hs_bindgen_test_arraysarray_e65f5d175373d7d0 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @arrays\/array.h:68:12@

__exported by:__ @arrays\/array.h@
-}
arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_e65f5d175373d7d0

{-| __unique:__ @Example_get_arr_4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_f5ff472666eacc51" hs_bindgen_test_arraysarray_f5ff472666eacc51 ::
     IO (Ptr.Ptr List)

{-# NOINLINE arr_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @arrays\/array.h:71:13@

__exported by:__ @arrays\/array.h@
-}
arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_f5ff472666eacc51

{-| __unique:__ @Example_get_arr_5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_ee4edbac53dee1f5" hs_bindgen_test_arraysarray_ee4edbac53dee1f5 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @arrays\/array.h:74:12@

__exported by:__ @arrays\/array.h@
-}
arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_ee4edbac53dee1f5

{-| __unique:__ @Example_get_arr_6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9b28f4801607118b" hs_bindgen_test_arraysarray_9b28f4801607118b ::
     IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @arrays\/array.h:77:15@

__exported by:__ @arrays\/array.h@
-}
arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9b28f4801607118b

{-| __unique:__ @Example_get_arr_7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d32b62c9cd2ceab5" hs_bindgen_test_arraysarray_d32b62c9cd2ceab5 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @arrays\/array.h:80:12@

__exported by:__ @arrays\/array.h@
-}
arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d32b62c9cd2ceab5

{-| __unique:__ @Example_get_arr_8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a300d3453acf6bf3" hs_bindgen_test_arraysarray_a300d3453acf6bf3 ::
     IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @arrays\/array.h:83:20@

__exported by:__ @arrays\/array.h@
-}
arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a300d3453acf6bf3

{-| __unique:__ @Example_get_arr_1_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a4aaceddda1cb5c1" hs_bindgen_test_arraysarray_a4aaceddda1cb5c1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @arrays\/array.h:90:18@

__exported by:__ @arrays\/array.h@
-}
arr_1_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a4aaceddda1cb5c1

{-# NOINLINE arr_1_const #-}

arr_1_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
arr_1_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_1_const_ptr)

{-| __unique:__ @Example_get_arr_2_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_b7b07f85effc1faa" hs_bindgen_test_arraysarray_b7b07f85effc1faa ::
     IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @arrays\/array.h:93:22@

__exported by:__ @arrays\/array.h@
-}
arr_2_const_ptr :: Ptr.Ptr Triplet
arr_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_b7b07f85effc1faa

{-# NOINLINE arr_2_const #-}

arr_2_const :: Triplet
arr_2_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_2_const_ptr)

{-| __unique:__ @Example_get_arr_3_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6110ddf1070fea5d" hs_bindgen_test_arraysarray_6110ddf1070fea5d ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @arrays\/array.h:96:18@

__exported by:__ @arrays\/array.h@
-}
arr_3_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6110ddf1070fea5d

{-| __unique:__ @Example_get_arr_4_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_c7988096e618383a" hs_bindgen_test_arraysarray_c7988096e618383a ::
     IO (Ptr.Ptr List)

{-# NOINLINE arr_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @arrays\/array.h:99:19@

__exported by:__ @arrays\/array.h@
-}
arr_4_const_ptr :: Ptr.Ptr List
arr_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_c7988096e618383a

{-| __unique:__ @Example_get_arr_5_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_ab0670fbdb25bf13" hs_bindgen_test_arraysarray_ab0670fbdb25bf13 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @arrays\/array.h:102:18@

__exported by:__ @arrays\/array.h@
-}
arr_5_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_ab0670fbdb25bf13

{-# NOINLINE arr_5_const #-}

arr_5_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_5_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_5_const_ptr)

{-| __unique:__ @Example_get_arr_6_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6f0b225d9fb26332" hs_bindgen_test_arraysarray_6f0b225d9fb26332 ::
     IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @arrays\/array.h:105:21@

__exported by:__ @arrays\/array.h@
-}
arr_6_const_ptr :: Ptr.Ptr Matrix
arr_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6f0b225d9fb26332

{-# NOINLINE arr_6_const #-}

arr_6_const :: Matrix
arr_6_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_6_const_ptr)

{-| __unique:__ @Example_get_arr_7_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6be26ad867a2255f" hs_bindgen_test_arraysarray_6be26ad867a2255f ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @arrays\/array.h:108:18@

__exported by:__ @arrays\/array.h@
-}
arr_7_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6be26ad867a2255f

{-| __unique:__ @Example_get_arr_8_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d2885ea24529e66c" hs_bindgen_test_arraysarray_d2885ea24529e66c ::
     IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @arrays\/array.h:111:26@

__exported by:__ @arrays\/array.h@
-}
arr_8_const_ptr :: Ptr.Ptr Tripletlist
arr_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d2885ea24529e66c
