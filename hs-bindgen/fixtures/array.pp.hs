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

$(CAPI.addCSource "#include <array.h>\nsigned int hs_bindgen_test_array_5d1be223fd040c3b (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }\nsigned int hs_bindgen_test_array_cabe35537b18e986 (signed int *arg1) { return fun_2(arg1); }\nsigned int hs_bindgen_test_array_4cdbf10236e78984 (signed int *arg1) { return fun_3(arg1); }\nsigned int hs_bindgen_test_array_e356c5ddb2608063 (signed int *arg1) { return fun_4(arg1); }\nsigned int hs_bindgen_test_array_f5ccf2c8d2e60be5 (signed int (*arg1)[3]) { return fun_5(arg1); }\nsigned int hs_bindgen_test_array_2b3a983697999524 (signed int (*arg1)[3]) { return fun_6(arg1); }\nsigned int hs_bindgen_test_array_72e9371a1b8b8907 (signed int (*arg1)[3]) { return fun_7(arg1); }\nsigned int hs_bindgen_test_array_62ad87463d9a75de (signed int (*arg1)[3]) { return fun_8(arg1); }\nsigned int (*hs_bindgen_test_array_d4c729a69c884fd4 (void))[3] { return fun_9(); }\ntriplet *hs_bindgen_test_array_bb92dfded907271e (void) { return fun_10(); }\nsigned int (*hs_bindgen_test_array_489aaaa59e992ddf (void))[] { return fun_11(); }\nlist *hs_bindgen_test_array_ee94c35f987d6c50 (void) { return fun_12(); }\nsigned int (*hs_bindgen_test_array_ca2c7b60ce85a964 (void))[4][3] { return fun_13(); }\nmatrix *hs_bindgen_test_array_ab2c533efdae8e41 (void) { return fun_14(); }\nsigned int (*hs_bindgen_test_array_019bdeb5db79cee1 (void))[][3] { return fun_15(); }\ntripletlist *hs_bindgen_test_array_ca0e7c51654fef12 (void) { return fun_16(); }\n/* get_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3ced2f3b2af806f8 (void)) (signed int arg1, signed int arg2[3]) { return &fun_1; } \n/* get_fun_2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_84966994a8d7df93 (void)) (triplet arg1) { return &fun_2; } \n/* get_fun_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3e6c940dbd7e5492 (void)) (signed int arg1[]) { return &fun_3; } \n/* get_fun_4_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_d9f87d3e541b15e5 (void)) (list arg1) { return &fun_4; } \n/* get_fun_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_cd41e41992d89300 (void)) (signed int arg1[4][3]) { return &fun_5; } \n/* get_fun_6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_db0e2655437ab8bb (void)) (matrix arg1) { return &fun_6; } \n/* get_fun_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_9ec02aa16b020aa0 (void)) (signed int arg1[][3]) { return &fun_7; } \n/* get_fun_8_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a41b8d1332b69b95 (void)) (tripletlist arg1) { return &fun_8; } \n/* get_fun_9_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_76f53f330102e743 (void)) (void))[3] { return &fun_9; } \n/* get_fun_10_ptr */ __attribute__ ((const)) triplet *(*hs_bindgen_test_array_abcc94f01de77b25 (void)) (void) { return &fun_10; } \n/* get_fun_11_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_6661b46e4a751a85 (void)) (void))[] { return &fun_11; } \n/* get_fun_12_ptr */ __attribute__ ((const)) list *(*hs_bindgen_test_array_9c80a9e3300aad15 (void)) (void) { return &fun_12; } \n/* get_fun_13_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_bb741b7e8c029e7e (void)) (void))[4][3] { return &fun_13; } \n/* get_fun_14_ptr */ __attribute__ ((const)) matrix *(*hs_bindgen_test_array_75d83252a55a5c64 (void)) (void) { return &fun_14; } \n/* get_fun_15_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_069ac2d1873f3210 (void)) (void))[][3] { return &fun_15; } \n/* get_fun_16_ptr */ __attribute__ ((const)) tripletlist *(*hs_bindgen_test_array_314971335aaa6db3 (void)) (void) { return &fun_16; } \n/* get_arr0_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a6413f4d2092265d (void))[3] { return &arr0; } \n/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1693226264ba4aeb (void))[3] { return &arr1; } \n/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_dafcf99a73b93389 (void))[3] { return &arr2; } \n/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_ca1016acc3449dee (void))[3] { return &arr3; } \n/* get_arr6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1a8c921160bc99a6 (void))[1] { return &arr6; } \n/* get_arr7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_17cf970243739b65 (void))[] { return &arr7; } \n/* get_arr_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_85bc33b188037456 (void))[3] { return &arr_1; } \n/* get_arr_2_ptr */ __attribute__ ((const)) triplet *hs_bindgen_test_array_87c784150cd3ff65 (void) { return &arr_2; } \n/* get_arr_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_e7b0de7633a7a62a (void))[] { return &arr_3; } \n/* get_arr_4_ptr */ __attribute__ ((const)) list *hs_bindgen_test_array_8fb64bc6c2bd4c73 (void) { return &arr_4; } \n/* get_arr_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_7348a94e6adce96e (void))[4][3] { return &arr_5; } \n/* get_arr_6_ptr */ __attribute__ ((const)) matrix *hs_bindgen_test_array_1308613140bb4b80 (void) { return &arr_6; } \n/* get_arr_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a060984b378ed676 (void))[][3] { return &arr_7; } \n/* get_arr_8_ptr */ __attribute__ ((const)) tripletlist *hs_bindgen_test_array_d82706abb6d8ea04 (void) { return &arr_8; } \n")

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

foreign import ccall safe "hs_bindgen_test_array_5d1be223fd040c3b" fun_1_wrapper
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

foreign import ccall safe "hs_bindgen_test_array_cabe35537b18e986" fun_2_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_2 :: Triplet -> IO FC.CInt
fun_2 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_2_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_4cdbf10236e78984" fun_3_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_3 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt
fun_3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_3_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_e356c5ddb2608063" fun_4_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

fun_4 :: List -> IO FC.CInt
fun_4 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_4_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_f5ccf2c8d2e60be5" fun_5_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_5 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_5_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_2b3a983697999524" fun_6_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_6 :: Matrix -> IO FC.CInt
fun_6 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_6_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_72e9371a1b8b8907" fun_7_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_7 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_7 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_7_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_62ad87463d9a75de" fun_8_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

fun_8 :: Tripletlist -> IO FC.CInt
fun_8 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_8_wrapper ptr1)

foreign import ccall safe "hs_bindgen_test_array_d4c729a69c884fd4" fun_9
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

foreign import ccall safe "hs_bindgen_test_array_bb92dfded907271e" fun_10
  :: IO (Ptr.Ptr Triplet)

foreign import ccall safe "hs_bindgen_test_array_489aaaa59e992ddf" fun_11
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

foreign import ccall safe "hs_bindgen_test_array_ee94c35f987d6c50" fun_12
  :: IO (Ptr.Ptr List)

foreign import ccall safe "hs_bindgen_test_array_ca2c7b60ce85a964" fun_13
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "hs_bindgen_test_array_ab2c533efdae8e41" fun_14
  :: IO (Ptr.Ptr Matrix)

foreign import ccall safe "hs_bindgen_test_array_019bdeb5db79cee1" fun_15
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

foreign import ccall safe "hs_bindgen_test_array_ca0e7c51654fef12" fun_16
  :: IO (Ptr.Ptr Tripletlist)

foreign import ccall unsafe "hs_bindgen_test_array_3ced2f3b2af806f8" hs_bindgen_test_array_3ced2f3b2af806f8
  :: IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_ptr #-}

fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3ced2f3b2af806f8

foreign import ccall unsafe "hs_bindgen_test_array_84966994a8d7df93" hs_bindgen_test_array_84966994a8d7df93
  :: IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_ptr #-}

fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_84966994a8d7df93

foreign import ccall unsafe "hs_bindgen_test_array_3e6c940dbd7e5492" hs_bindgen_test_array_3e6c940dbd7e5492
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_ptr #-}

fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3e6c940dbd7e5492

foreign import ccall unsafe "hs_bindgen_test_array_d9f87d3e541b15e5" hs_bindgen_test_array_d9f87d3e541b15e5
  :: IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4_ptr #-}

fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_d9f87d3e541b15e5

foreign import ccall unsafe "hs_bindgen_test_array_cd41e41992d89300" hs_bindgen_test_array_cd41e41992d89300
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_ptr #-}

fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_cd41e41992d89300

foreign import ccall unsafe "hs_bindgen_test_array_db0e2655437ab8bb" hs_bindgen_test_array_db0e2655437ab8bb
  :: IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_ptr #-}

fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_db0e2655437ab8bb

foreign import ccall unsafe "hs_bindgen_test_array_9ec02aa16b020aa0" hs_bindgen_test_array_9ec02aa16b020aa0
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_ptr #-}

fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9ec02aa16b020aa0

foreign import ccall unsafe "hs_bindgen_test_array_a41b8d1332b69b95" hs_bindgen_test_array_a41b8d1332b69b95
  :: IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_ptr #-}

fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a41b8d1332b69b95

foreign import ccall unsafe "hs_bindgen_test_array_76f53f330102e743" hs_bindgen_test_array_76f53f330102e743
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9_ptr #-}

fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_76f53f330102e743

foreign import ccall unsafe "hs_bindgen_test_array_abcc94f01de77b25" hs_bindgen_test_array_abcc94f01de77b25
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10_ptr #-}

fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_abcc94f01de77b25

foreign import ccall unsafe "hs_bindgen_test_array_6661b46e4a751a85" hs_bindgen_test_array_6661b46e4a751a85
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11_ptr #-}

fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_6661b46e4a751a85

foreign import ccall unsafe "hs_bindgen_test_array_9c80a9e3300aad15" hs_bindgen_test_array_9c80a9e3300aad15
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12_ptr #-}

fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9c80a9e3300aad15

foreign import ccall unsafe "hs_bindgen_test_array_bb741b7e8c029e7e" hs_bindgen_test_array_bb741b7e8c029e7e
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13_ptr #-}

fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_bb741b7e8c029e7e

foreign import ccall unsafe "hs_bindgen_test_array_75d83252a55a5c64" hs_bindgen_test_array_75d83252a55a5c64
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14_ptr #-}

fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_75d83252a55a5c64

foreign import ccall unsafe "hs_bindgen_test_array_069ac2d1873f3210" hs_bindgen_test_array_069ac2d1873f3210
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15_ptr #-}

fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_069ac2d1873f3210

foreign import ccall unsafe "hs_bindgen_test_array_314971335aaa6db3" hs_bindgen_test_array_314971335aaa6db3
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16_ptr #-}

fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_314971335aaa6db3

foreign import ccall unsafe "hs_bindgen_test_array_a6413f4d2092265d" hs_bindgen_test_array_a6413f4d2092265d
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0_ptr #-}

arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a6413f4d2092265d

foreign import ccall unsafe "hs_bindgen_test_array_1693226264ba4aeb" hs_bindgen_test_array_1693226264ba4aeb
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1_ptr #-}

arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1693226264ba4aeb

foreign import ccall unsafe "hs_bindgen_test_array_dafcf99a73b93389" hs_bindgen_test_array_dafcf99a73b93389
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_dafcf99a73b93389

foreign import ccall unsafe "hs_bindgen_test_array_ca1016acc3449dee" hs_bindgen_test_array_ca1016acc3449dee
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3_ptr #-}

arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_ca1016acc3449dee

foreign import ccall unsafe "hs_bindgen_test_array_1a8c921160bc99a6" hs_bindgen_test_array_1a8c921160bc99a6
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6_ptr #-}

arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1a8c921160bc99a6

foreign import ccall unsafe "hs_bindgen_test_array_17cf970243739b65" hs_bindgen_test_array_17cf970243739b65
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7_ptr #-}

arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_17cf970243739b65

foreign import ccall unsafe "hs_bindgen_test_array_85bc33b188037456" hs_bindgen_test_array_85bc33b188037456
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_ptr #-}

arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_85bc33b188037456

foreign import ccall unsafe "hs_bindgen_test_array_87c784150cd3ff65" hs_bindgen_test_array_87c784150cd3ff65
  :: IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_ptr #-}

arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_87c784150cd3ff65

foreign import ccall unsafe "hs_bindgen_test_array_e7b0de7633a7a62a" hs_bindgen_test_array_e7b0de7633a7a62a
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_ptr #-}

arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_e7b0de7633a7a62a

foreign import ccall unsafe "hs_bindgen_test_array_8fb64bc6c2bd4c73" hs_bindgen_test_array_8fb64bc6c2bd4c73
  :: IO (Ptr.Ptr List)

{-# NOINLINE arr_4_ptr #-}

arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_8fb64bc6c2bd4c73

foreign import ccall unsafe "hs_bindgen_test_array_7348a94e6adce96e" hs_bindgen_test_array_7348a94e6adce96e
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_ptr #-}

arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_7348a94e6adce96e

foreign import ccall unsafe "hs_bindgen_test_array_1308613140bb4b80" hs_bindgen_test_array_1308613140bb4b80
  :: IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_ptr #-}

arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1308613140bb4b80

foreign import ccall unsafe "hs_bindgen_test_array_a060984b378ed676" hs_bindgen_test_array_a060984b378ed676
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_ptr #-}

arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a060984b378ed676

foreign import ccall unsafe "hs_bindgen_test_array_d82706abb6d8ea04" hs_bindgen_test_array_d82706abb6d8ea04
  :: IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_ptr #-}

arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_d82706abb6d8ea04
