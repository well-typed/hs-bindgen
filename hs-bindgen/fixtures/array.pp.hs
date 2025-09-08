{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <array.h>\n/* get_arr0_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_5c54826466f2e87b (void))[3] { return &arr0; } \n/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_ec6a481a47ca4eb1 (void))[3] { return &arr1; } \n/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_34db8d8b69220fcc (void))[3] { return &arr2; } \n/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_f4e746193b856003 (void))[3] { return &arr3; } \n/* get_arr6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_bf91904b3049fdd2 (void))[1] { return &arr6; } \n/* get_arr7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_9be06c66ecc3a933 (void))[] { return &arr7; } \n/* get_arr_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_16bca3ac468967d9 (void))[3] { return &arr_1; } \n/* get_arr_2_ptr */ __attribute__ ((const)) triplet *hs_bindgen_test_array_07e58c5432be4a35 (void) { return &arr_2; } \n/* get_arr_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_63e072530b04d3b9 (void))[] { return &arr_3; } \n/* get_arr_4_ptr */ __attribute__ ((const)) list *hs_bindgen_test_array_3db8d1257bc10233 (void) { return &arr_4; } \n/* get_arr_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_0f74917ee2000dc5 (void))[4][3] { return &arr_5; } \n/* get_arr_6_ptr */ __attribute__ ((const)) matrix *hs_bindgen_test_array_a48940bd219530d0 (void) { return &arr_6; } \n/* get_arr_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1196efca365094f7 (void))[][3] { return &arr_7; } \n/* get_arr_8_ptr */ __attribute__ ((const)) tripletlist *hs_bindgen_test_array_31b6cf83380518c3 (void) { return &arr_8; } \nsigned int hs_bindgen_test_array_c3dd28889d5b2858 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }\n/* get_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_93382555f3789d90 (void)) (signed int arg1, signed int arg2[3]) { return &fun_1; } \nsigned int hs_bindgen_test_array_3a8794adaf677495 (signed int *arg1) { return fun_2(arg1); }\n/* get_fun_2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_4afbbb57351eed54 (void)) (triplet arg1) { return &fun_2; } \nsigned int hs_bindgen_test_array_9d59f0f165a2f5cf (signed int *arg1) { return fun_3(arg1); }\n/* get_fun_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3f63bc5e6ab55421 (void)) (signed int arg1[]) { return &fun_3; } \nsigned int hs_bindgen_test_array_c650da1d5d7cf63d (signed int *arg1) { return fun_4(arg1); }\n/* get_fun_4_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_41b8a0c3ba0ec9e1 (void)) (list arg1) { return &fun_4; } \nsigned int hs_bindgen_test_array_cfa709c8c74d1eb7 (signed int (*arg1)[3]) { return fun_5(arg1); }\n/* get_fun_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_d1abe7db131e3b19 (void)) (signed int arg1[4][3]) { return &fun_5; } \nsigned int hs_bindgen_test_array_0432f7eb5cf9a91a (signed int (*arg1)[3]) { return fun_6(arg1); }\n/* get_fun_6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_7c972f297f91d659 (void)) (matrix arg1) { return &fun_6; } \nsigned int hs_bindgen_test_array_8dff35caae296df4 (signed int (*arg1)[3]) { return fun_7(arg1); }\n/* get_fun_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_6b3f3d6a5093e5cc (void)) (signed int arg1[][3]) { return &fun_7; } \nsigned int hs_bindgen_test_array_b7ed421e8d20e910 (signed int (*arg1)[3]) { return fun_8(arg1); }\n/* get_fun_8_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_c26b88304638d612 (void)) (tripletlist arg1) { return &fun_8; } \nsigned int (*hs_bindgen_test_array_c189d8ac4373a49e (void))[3] { return fun_9(); }\n/* get_fun_9_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_a9f13cbd15057404 (void)) (void))[3] { return &fun_9; } \ntriplet *hs_bindgen_test_array_8c628ce09b6680aa (void) { return fun_10(); }\n/* get_fun_10_ptr */ __attribute__ ((const)) triplet *(*hs_bindgen_test_array_e0071a6c4a5b4bfc (void)) (void) { return &fun_10; } \nsigned int (*hs_bindgen_test_array_1b95257f679dfafa (void))[] { return fun_11(); }\n/* get_fun_11_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_c242f0482c8c80e3 (void)) (void))[] { return &fun_11; } \nlist *hs_bindgen_test_array_41c7417f447a711d (void) { return fun_12(); }\n/* get_fun_12_ptr */ __attribute__ ((const)) list *(*hs_bindgen_test_array_29c3e7c11a23c44f (void)) (void) { return &fun_12; } \nsigned int (*hs_bindgen_test_array_752ccb7f8f99f330 (void))[4][3] { return fun_13(); }\n/* get_fun_13_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_c30eaebfadb99814 (void)) (void))[4][3] { return &fun_13; } \nmatrix *hs_bindgen_test_array_6f6335641a10b824 (void) { return fun_14(); }\n/* get_fun_14_ptr */ __attribute__ ((const)) matrix *(*hs_bindgen_test_array_51fe3407a9c308d6 (void)) (void) { return &fun_14; } \nsigned int (*hs_bindgen_test_array_22a4737ff5651e15 (void))[][3] { return fun_15(); }\n/* get_fun_15_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_74204588fdafa1a9 (void)) (void))[][3] { return &fun_15; } \ntripletlist *hs_bindgen_test_array_bce26548e8519620 (void) { return fun_16(); }\n/* get_fun_16_ptr */ __attribute__ ((const)) tripletlist *(*hs_bindgen_test_array_58a2b1249489d65b (void)) (void) { return &fun_16; } \n")

foreign import ccall unsafe "hs_bindgen_test_array_5c54826466f2e87b" hs_bindgen_test_array_5c54826466f2e87b
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0_ptr #-}

arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_5c54826466f2e87b

foreign import ccall unsafe "hs_bindgen_test_array_ec6a481a47ca4eb1" hs_bindgen_test_array_ec6a481a47ca4eb1
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1_ptr #-}

arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_ec6a481a47ca4eb1

foreign import ccall unsafe "hs_bindgen_test_array_34db8d8b69220fcc" hs_bindgen_test_array_34db8d8b69220fcc
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_34db8d8b69220fcc

foreign import ccall unsafe "hs_bindgen_test_array_f4e746193b856003" hs_bindgen_test_array_f4e746193b856003
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3_ptr #-}

arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_f4e746193b856003

foreign import ccall unsafe "hs_bindgen_test_array_bf91904b3049fdd2" hs_bindgen_test_array_bf91904b3049fdd2
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6_ptr #-}

arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_bf91904b3049fdd2

foreign import ccall unsafe "hs_bindgen_test_array_9be06c66ecc3a933" hs_bindgen_test_array_9be06c66ecc3a933
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7_ptr #-}

arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9be06c66ecc3a933

{-| __C declaration:__ @triplet@

    __defined at:__ @array.h:41:13@

    __exported by:__ @array.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @list@

    __defined at:__ @array.h:43:13@

    __exported by:__ @array.h@
-}
newtype List = List
  { un_List :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @matrix@

    __defined at:__ @array.h:45:13@

    __exported by:__ @array.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| __C declaration:__ @tripletlist@

    __defined at:__ @array.h:47:13@

    __exported by:__ @array.h@
-}
newtype Tripletlist = Tripletlist
  { un_Tripletlist :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @Example@

    __defined at:__ @array.h:49:8@

    __exported by:__ @array.h@
-}
data Example = Example
  { example_triple :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @triple@

         __defined at:__ @array.h:50:9@

         __exported by:__ @array.h@
    -}
  , example_sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    {- ^ __C declaration:__ @sudoku@

         __defined at:__ @array.h:51:9@

         __exported by:__ @array.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Example where

  sizeOf = \_ -> (48 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Example
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Example example_triple2 example_sudoku3 ->
               F.pokeByteOff ptr0 (0 :: Int) example_triple2
            >> F.pokeByteOff ptr0 (12 :: Int) example_sudoku3

foreign import ccall unsafe "hs_bindgen_test_array_16bca3ac468967d9" hs_bindgen_test_array_16bca3ac468967d9
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_ptr #-}

arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_16bca3ac468967d9

foreign import ccall unsafe "hs_bindgen_test_array_07e58c5432be4a35" hs_bindgen_test_array_07e58c5432be4a35
  :: IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_ptr #-}

arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_07e58c5432be4a35

foreign import ccall unsafe "hs_bindgen_test_array_63e072530b04d3b9" hs_bindgen_test_array_63e072530b04d3b9
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_ptr #-}

arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_63e072530b04d3b9

foreign import ccall unsafe "hs_bindgen_test_array_3db8d1257bc10233" hs_bindgen_test_array_3db8d1257bc10233
  :: IO (Ptr.Ptr List)

{-# NOINLINE arr_4_ptr #-}

arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3db8d1257bc10233

foreign import ccall unsafe "hs_bindgen_test_array_0f74917ee2000dc5" hs_bindgen_test_array_0f74917ee2000dc5
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_ptr #-}

arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_0f74917ee2000dc5

foreign import ccall unsafe "hs_bindgen_test_array_a48940bd219530d0" hs_bindgen_test_array_a48940bd219530d0
  :: IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_ptr #-}

arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a48940bd219530d0

foreign import ccall unsafe "hs_bindgen_test_array_1196efca365094f7" hs_bindgen_test_array_1196efca365094f7
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_ptr #-}

arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1196efca365094f7

foreign import ccall unsafe "hs_bindgen_test_array_31b6cf83380518c3" hs_bindgen_test_array_31b6cf83380518c3
  :: IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_ptr #-}

arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_31b6cf83380518c3

foreign import ccall safe "hs_bindgen_test_array_c3dd28889d5b2858" fun_1_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_1 :: FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt
fun_1 =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_1_wrapper x0 ptr2)

foreign import ccall unsafe "hs_bindgen_test_array_93382555f3789d90" hs_bindgen_test_array_93382555f3789d90
  :: IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_ptr #-}

fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_93382555f3789d90

foreign import ccall safe "hs_bindgen_test_array_3a8794adaf677495" fun_2_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_2 :: Triplet -> IO FC.CInt
fun_2 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_2_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_4afbbb57351eed54" hs_bindgen_test_array_4afbbb57351eed54
  :: IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_ptr #-}

fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_4afbbb57351eed54

foreign import ccall safe "hs_bindgen_test_array_9d59f0f165a2f5cf" fun_3_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_3 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt
fun_3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_3_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_3f63bc5e6ab55421" hs_bindgen_test_array_3f63bc5e6ab55421
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_ptr #-}

fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3f63bc5e6ab55421

foreign import ccall safe "hs_bindgen_test_array_c650da1d5d7cf63d" fun_4_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_4 :: List -> IO FC.CInt
fun_4 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_4_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_41b8a0c3ba0ec9e1" hs_bindgen_test_array_41b8a0c3ba0ec9e1
  :: IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4_ptr #-}

fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_41b8a0c3ba0ec9e1

foreign import ccall safe "hs_bindgen_test_array_cfa709c8c74d1eb7" fun_5_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_5 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_5_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_d1abe7db131e3b19" hs_bindgen_test_array_d1abe7db131e3b19
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_ptr #-}

fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_d1abe7db131e3b19

foreign import ccall safe "hs_bindgen_test_array_0432f7eb5cf9a91a" fun_6_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_6 :: Matrix -> IO FC.CInt
fun_6 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_6_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_7c972f297f91d659" hs_bindgen_test_array_7c972f297f91d659
  :: IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_ptr #-}

fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_7c972f297f91d659

foreign import ccall safe "hs_bindgen_test_array_8dff35caae296df4" fun_7_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_7 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_7 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_7_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_6b3f3d6a5093e5cc" hs_bindgen_test_array_6b3f3d6a5093e5cc
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_ptr #-}

fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_6b3f3d6a5093e5cc

foreign import ccall safe "hs_bindgen_test_array_b7ed421e8d20e910" fun_8_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_8 :: Tripletlist -> IO FC.CInt
fun_8 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_8_wrapper ptr1)

foreign import ccall unsafe "hs_bindgen_test_array_c26b88304638d612" hs_bindgen_test_array_c26b88304638d612
  :: IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_ptr #-}

fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_c26b88304638d612

foreign import ccall safe "hs_bindgen_test_array_c189d8ac4373a49e" fun_9
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

foreign import ccall unsafe "hs_bindgen_test_array_a9f13cbd15057404" hs_bindgen_test_array_a9f13cbd15057404
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9_ptr #-}

fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a9f13cbd15057404

foreign import ccall safe "hs_bindgen_test_array_8c628ce09b6680aa" fun_10
  :: IO (Ptr.Ptr Triplet)

foreign import ccall unsafe "hs_bindgen_test_array_e0071a6c4a5b4bfc" hs_bindgen_test_array_e0071a6c4a5b4bfc
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10_ptr #-}

fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_e0071a6c4a5b4bfc

foreign import ccall safe "hs_bindgen_test_array_1b95257f679dfafa" fun_11
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

foreign import ccall unsafe "hs_bindgen_test_array_c242f0482c8c80e3" hs_bindgen_test_array_c242f0482c8c80e3
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11_ptr #-}

fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_c242f0482c8c80e3

foreign import ccall safe "hs_bindgen_test_array_41c7417f447a711d" fun_12
  :: IO (Ptr.Ptr List)

foreign import ccall unsafe "hs_bindgen_test_array_29c3e7c11a23c44f" hs_bindgen_test_array_29c3e7c11a23c44f
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12_ptr #-}

fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_29c3e7c11a23c44f

foreign import ccall safe "hs_bindgen_test_array_752ccb7f8f99f330" fun_13
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall unsafe "hs_bindgen_test_array_c30eaebfadb99814" hs_bindgen_test_array_c30eaebfadb99814
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13_ptr #-}

fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_c30eaebfadb99814

foreign import ccall safe "hs_bindgen_test_array_6f6335641a10b824" fun_14
  :: IO (Ptr.Ptr Matrix)

foreign import ccall unsafe "hs_bindgen_test_array_51fe3407a9c308d6" hs_bindgen_test_array_51fe3407a9c308d6
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14_ptr #-}

fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_51fe3407a9c308d6

foreign import ccall safe "hs_bindgen_test_array_22a4737ff5651e15" fun_15
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall unsafe "hs_bindgen_test_array_74204588fdafa1a9" hs_bindgen_test_array_74204588fdafa1a9
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15_ptr #-}

fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_74204588fdafa1a9

foreign import ccall safe "hs_bindgen_test_array_bce26548e8519620" fun_16
  :: IO (Ptr.Ptr Tripletlist)

foreign import ccall unsafe "hs_bindgen_test_array_58a2b1249489d65b" hs_bindgen_test_array_58a2b1249489d65b
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16_ptr #-}

fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_58a2b1249489d65b
