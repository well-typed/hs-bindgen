{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(HsBindgen.Runtime.Prelude.addCSource "#include <array.h>\nsigned int hs_bindgen_test_array_5d1be223fd040c3b (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }\nsigned int hs_bindgen_test_array_cabe35537b18e986 (signed int *arg1) { return fun_2(arg1); }\nsigned int hs_bindgen_test_array_4cdbf10236e78984 (signed int *arg1) { return fun_3(arg1); }\nsigned int hs_bindgen_test_array_e356c5ddb2608063 (signed int *arg1) { return fun_4(arg1); }\nsigned int hs_bindgen_test_array_f5ccf2c8d2e60be5 (signed int (*arg1)[3]) { return fun_5(arg1); }\nsigned int hs_bindgen_test_array_2b3a983697999524 (signed int (*arg1)[3]) { return fun_6(arg1); }\nsigned int hs_bindgen_test_array_72e9371a1b8b8907 (signed int (*arg1)[3]) { return fun_7(arg1); }\nsigned int hs_bindgen_test_array_62ad87463d9a75de (signed int (*arg1)[3]) { return fun_8(arg1); }\nsigned int hs_bindgen_test_array_2280ecc4c152a73f (triplet *arg1) { return isSolved(arg1); }\nsigned int hs_bindgen_test_array_f1d120f83dc61db5 (signed int arg1, signed int *arg2, signed int const *arg3) { return fun_1_const(arg1, arg2, arg3); }\nsigned int hs_bindgen_test_array_f15760e6f3596189 (signed int *arg1, triplet const arg2) { return fun_2_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_0ad99f041fc4f5ca (signed int *arg1, signed int const *arg2) { return fun_3_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_d61f2b8777e6ca19 (signed int *arg1, list const arg2) { return fun_4_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_9e1f66e6a0369c45 (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_5_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_5b4bd3c6cee83e61 (signed int (*arg1)[3], matrix const arg2) { return fun_6_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_b551069ce9e1f12e (signed int (*arg1)[3], signed int const (*arg2)[3]) { return fun_7_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_4ac495707a95aa13 (signed int (*arg1)[3], tripletlist const arg2) { return fun_8_const(arg1, arg2); }\nsigned int hs_bindgen_test_array_1bdcfcd7aca9a2f6 (triplet *arg1, sudoku const arg2) { return isSolved_const(arg1, arg2); }\nsigned int (*hs_bindgen_test_array_d4c729a69c884fd4 (void))[3] { return fun_9(); }\ntriplet *hs_bindgen_test_array_bb92dfded907271e (void) { return fun_10(); }\nsigned int (*hs_bindgen_test_array_489aaaa59e992ddf (void))[] { return fun_11(); }\nlist *hs_bindgen_test_array_ee94c35f987d6c50 (void) { return fun_12(); }\nsigned int (*hs_bindgen_test_array_ca2c7b60ce85a964 (void))[4][3] { return fun_13(); }\nmatrix *hs_bindgen_test_array_ab2c533efdae8e41 (void) { return fun_14(); }\nsigned int (*hs_bindgen_test_array_019bdeb5db79cee1 (void))[][3] { return fun_15(); }\ntripletlist *hs_bindgen_test_array_ca0e7c51654fef12 (void) { return fun_16(); }\nsudoku *hs_bindgen_test_array_f6b66497ee1685b0 (void) { return solve(); }\n/* get_fun_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3ced2f3b2af806f8 (void)) (signed int arg1, signed int arg2[3]) { return &fun_1; } \n/* get_fun_2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_84966994a8d7df93 (void)) (triplet arg1) { return &fun_2; } \n/* get_fun_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3e6c940dbd7e5492 (void)) (signed int arg1[]) { return &fun_3; } \n/* get_fun_4_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_d9f87d3e541b15e5 (void)) (list arg1) { return &fun_4; } \n/* get_fun_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_cd41e41992d89300 (void)) (signed int arg1[4][3]) { return &fun_5; } \n/* get_fun_6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_db0e2655437ab8bb (void)) (matrix arg1) { return &fun_6; } \n/* get_fun_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_9ec02aa16b020aa0 (void)) (signed int arg1[][3]) { return &fun_7; } \n/* get_fun_8_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a41b8d1332b69b95 (void)) (tripletlist arg1) { return &fun_8; } \n/* get_isSolved_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_bdf2a6a8a3dd5b04 (void)) (sudoku arg1) { return &isSolved; } \n/* get_fun_1_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a3de5f7e233ad0e1 (void)) (signed int arg1, signed int arg2[3], signed int const arg3[3]) { return &fun_1_const; } \n/* get_fun_2_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3c09bbba7534ca1d (void)) (triplet arg1, triplet const arg2) { return &fun_2_const; } \n/* get_fun_3_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_0e53ed28ec1ca276 (void)) (signed int arg1[], signed int const arg2[]) { return &fun_3_const; } \n/* get_fun_4_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_07d860d5e74c415b (void)) (list arg1, list const arg2) { return &fun_4_const; } \n/* get_fun_5_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_3c0a139c24d7202a (void)) (signed int arg1[4][3], signed int const arg2[4][3]) { return &fun_5_const; } \n/* get_fun_6_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_62d236581cc18366 (void)) (matrix arg1, matrix const arg2) { return &fun_6_const; } \n/* get_fun_7_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_b4bf67c3cec12e54 (void)) (signed int arg1[][3], signed int const arg2[][3]) { return &fun_7_const; } \n/* get_fun_8_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_99dd6a6017eb0eec (void)) (tripletlist arg1, tripletlist const arg2) { return &fun_8_const; } \n/* get_isSolved_const_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_6deec046c95e4e0d (void)) (sudoku arg1, sudoku const arg2) { return &isSolved_const; } \n/* get_fun_9_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_76f53f330102e743 (void)) (void))[3] { return &fun_9; } \n/* get_fun_10_ptr */ __attribute__ ((const)) triplet *(*hs_bindgen_test_array_abcc94f01de77b25 (void)) (void) { return &fun_10; } \n/* get_fun_11_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_6661b46e4a751a85 (void)) (void))[] { return &fun_11; } \n/* get_fun_12_ptr */ __attribute__ ((const)) list *(*hs_bindgen_test_array_9c80a9e3300aad15 (void)) (void) { return &fun_12; } \n/* get_fun_13_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_bb741b7e8c029e7e (void)) (void))[4][3] { return &fun_13; } \n/* get_fun_14_ptr */ __attribute__ ((const)) matrix *(*hs_bindgen_test_array_75d83252a55a5c64 (void)) (void) { return &fun_14; } \n/* get_fun_15_ptr */ __attribute__ ((const)) signed int (*(*hs_bindgen_test_array_069ac2d1873f3210 (void)) (void))[][3] { return &fun_15; } \n/* get_fun_16_ptr */ __attribute__ ((const)) tripletlist *(*hs_bindgen_test_array_314971335aaa6db3 (void)) (void) { return &fun_16; } \n/* get_solve_ptr */ __attribute__ ((const)) sudoku *(*hs_bindgen_test_array_9a62b5848be64bd4 (void)) (void) { return &solve; } \n/* get_arr0_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a6413f4d2092265d (void))[3] { return &arr0; } \n/* get_arr1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1693226264ba4aeb (void))[3] { return &arr1; } \n/* get_arr2_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_dafcf99a73b93389 (void))[3] { return &arr2; } \n/* get_arr3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_ca1016acc3449dee (void))[3] { return &arr3; } \n/* get_arr6_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_1a8c921160bc99a6 (void))[1] { return &arr6; } \n/* get_arr7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_17cf970243739b65 (void))[] { return &arr7; } \n/* get_arr_1_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_85bc33b188037456 (void))[3] { return &arr_1; } \n/* get_arr_2_ptr */ __attribute__ ((const)) triplet *hs_bindgen_test_array_87c784150cd3ff65 (void) { return &arr_2; } \n/* get_arr_3_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_e7b0de7633a7a62a (void))[] { return &arr_3; } \n/* get_arr_4_ptr */ __attribute__ ((const)) list *hs_bindgen_test_array_8fb64bc6c2bd4c73 (void) { return &arr_4; } \n/* get_arr_5_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_7348a94e6adce96e (void))[4][3] { return &arr_5; } \n/* get_arr_6_ptr */ __attribute__ ((const)) matrix *hs_bindgen_test_array_1308613140bb4b80 (void) { return &arr_6; } \n/* get_arr_7_ptr */ __attribute__ ((const)) signed int (*hs_bindgen_test_array_a060984b378ed676 (void))[][3] { return &arr_7; } \n/* get_arr_8_ptr */ __attribute__ ((const)) tripletlist *hs_bindgen_test_array_d82706abb6d8ea04 (void) { return &arr_8; } \n/* get_arr_1_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_7376d172f5729493 (void))[3] { return &arr_1_const; } \n/* get_arr_2_const_ptr */ __attribute__ ((const)) triplet const *hs_bindgen_test_array_f03586aa57dfce29 (void) { return &arr_2_const; } \n/* get_arr_3_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_54ffd4ffcd2dad61 (void))[] { return &arr_3_const; } \n/* get_arr_4_const_ptr */ __attribute__ ((const)) list const *hs_bindgen_test_array_8896c2ff5b9ce9c9 (void) { return &arr_4_const; } \n/* get_arr_5_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_46b406e096f6c9c1 (void))[4][3] { return &arr_5_const; } \n/* get_arr_6_const_ptr */ __attribute__ ((const)) matrix const *hs_bindgen_test_array_ceb7f2027865ce12 (void) { return &arr_6_const; } \n/* get_arr_7_const_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_array_2b565b2b97acdcb7 (void))[][3] { return &arr_7_const; } \n/* get_arr_8_const_ptr */ __attribute__ ((const)) tripletlist const *hs_bindgen_test_array_03e2d9c4ef2ae993 (void) { return &arr_8_const; } \n")

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

{-| Typedef-in-typedef

__C declaration:__ @sudoku@

__defined at:__ @array.h:55:17@

__exported by:__ @array.h@
-}
newtype Sudoku = Sudoku
  { un_Sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @array.h:118:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_5d1be223fd040c3b" fun_1_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @array.h:118:5@

__exported by:__ @array.h@
-}
fun_1 :: FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt
fun_1 =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_1_wrapper x0 ptr2)

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @array.h:121:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_cabe35537b18e986" fun_2_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @array.h:121:5@

__exported by:__ @array.h@
-}
fun_2 :: Triplet -> IO FC.CInt
fun_2 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_2_wrapper ptr1)

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @array.h:124:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_4cdbf10236e78984" fun_3_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @array.h:124:5@

__exported by:__ @array.h@
-}
fun_3 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt
fun_3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_3_wrapper ptr1)

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @array.h:127:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_e356c5ddb2608063" fun_4_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @array.h:127:5@

__exported by:__ @array.h@
-}
fun_4 :: List -> IO FC.CInt
fun_4 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_4_wrapper ptr1)

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @array.h:130:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_f5ccf2c8d2e60be5" fun_5_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @array.h:130:5@

__exported by:__ @array.h@
-}
fun_5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_5 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_5_wrapper ptr1)

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @array.h:133:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_2b3a983697999524" fun_6_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @array.h:133:5@

__exported by:__ @array.h@
-}
fun_6 :: Matrix -> IO FC.CInt
fun_6 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_6_wrapper ptr1)

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @array.h:136:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_72e9371a1b8b8907" fun_7_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @array.h:136:5@

__exported by:__ @array.h@
-}
fun_7 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_7 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_7_wrapper ptr1)

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @array.h:139:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_62ad87463d9a75de" fun_8_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @array.h:139:5@

__exported by:__ @array.h@
-}
fun_8 :: Tripletlist -> IO FC.CInt
fun_8 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_8_wrapper ptr1)

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @array.h:142:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_2280ecc4c152a73f" isSolved_wrapper
  :: Ptr.Ptr Triplet
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @array.h:142:5@

__exported by:__ @array.h@
-}
isSolved :: Sudoku -> IO FC.CInt
isSolved =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  isSolved_wrapper ptr1)

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @array.h:149:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_f1d120f83dc61db5" fun_1_const_wrapper
  :: FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @array.h:149:5@

__exported by:__ @array.h@
-}
fun_1_const :: FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt
fun_1_const =
  \x0 ->
    \x1 ->
      \x2 ->
        HsBindgen.Runtime.ConstantArray.withPtr x2 (\ptr3 ->
                                                      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr4 ->
                                                                                                    fun_1_const_wrapper x0 ptr4 ptr3))

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @array.h:152:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_f15760e6f3596189" fun_2_const_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> Triplet
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @array.h:152:5@

__exported by:__ @array.h@
-}
fun_2_const :: Triplet -> Triplet -> IO FC.CInt
fun_2_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    fun_2_const_wrapper ptr2 x1)

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @array.h:155:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_0ad99f041fc4f5ca" fun_3_const_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @array.h:155:5@

__exported by:__ @array.h@
-}
fun_3_const :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt
fun_3_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr3 ->
                                                                                                      fun_3_const_wrapper ptr3 ptr2))

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @array.h:158:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_d61f2b8777e6ca19" fun_4_const_wrapper
  :: Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> List
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @array.h:158:5@

__exported by:__ @array.h@
-}
fun_4_const :: List -> List -> IO FC.CInt
fun_4_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr2 ->
                                                      fun_4_const_wrapper ptr2 x1)

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @array.h:161:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_9e1f66e6a0369c45" fun_5_const_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @array.h:161:5@

__exported by:__ @array.h@
-}
fun_5_const :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_5_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr3 ->
                                                                                                  fun_5_const_wrapper ptr3 ptr2))

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @array.h:164:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_5b4bd3c6cee83e61" fun_6_const_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Matrix
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @array.h:164:5@

__exported by:__ @array.h@
-}
fun_6_const :: Matrix -> Matrix -> IO FC.CInt
fun_6_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    fun_6_const_wrapper ptr2 x1)

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @array.h:167:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_b551069ce9e1f12e" fun_7_const_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @array.h:167:5@

__exported by:__ @array.h@
-}
fun_7_const :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_7_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr3 ->
                                                                                                      fun_7_const_wrapper ptr3 ptr2))

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @array.h:170:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_4ac495707a95aa13" fun_8_const_wrapper
  :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Tripletlist
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @array.h:170:5@

__exported by:__ @array.h@
-}
fun_8_const :: Tripletlist -> Tripletlist -> IO FC.CInt
fun_8_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr2 ->
                                                      fun_8_const_wrapper ptr2 x1)

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @array.h:173:5@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_1bdcfcd7aca9a2f6" isSolved_const_wrapper
  :: Ptr.Ptr Triplet
     {- ^ __C declaration:__ @xss@
     -}
  -> Sudoku
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @array.h:173:5@

__exported by:__ @array.h@
-}
isSolved_const :: Sudoku -> Sudoku -> IO FC.CInt
isSolved_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr2 ->
                                                    isSolved_const_wrapper ptr2 x1)

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @array.h:185:7@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_d4c729a69c884fd4" fun_9
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @array.h:188:10@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_bb92dfded907271e" fun_10
  :: IO (Ptr.Ptr Triplet)

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @array.h:191:7@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_489aaaa59e992ddf" fun_11
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @array.h:194:7@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_ee94c35f987d6c50" fun_12
  :: IO (Ptr.Ptr List)

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @array.h:197:7@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_ca2c7b60ce85a964" fun_13
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @array.h:200:9@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_ab2c533efdae8e41" fun_14
  :: IO (Ptr.Ptr Matrix)

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @array.h:203:7@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_019bdeb5db79cee1" fun_15
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @array.h:206:14@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_ca0e7c51654fef12" fun_16
  :: IO (Ptr.Ptr Tripletlist)

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @array.h:209:10@

__exported by:__ @array.h@
-}
foreign import ccall safe "hs_bindgen_test_array_f6b66497ee1685b0" solve
  :: IO (Ptr.Ptr Sudoku)

foreign import ccall unsafe "hs_bindgen_test_array_3ced2f3b2af806f8" hs_bindgen_test_array_3ced2f3b2af806f8
  :: IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @array.h:118:5@

__exported by:__ @array.h@
-}
fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3ced2f3b2af806f8

foreign import ccall unsafe "hs_bindgen_test_array_84966994a8d7df93" hs_bindgen_test_array_84966994a8d7df93
  :: IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @array.h:121:5@

__exported by:__ @array.h@
-}
fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_84966994a8d7df93

foreign import ccall unsafe "hs_bindgen_test_array_3e6c940dbd7e5492" hs_bindgen_test_array_3e6c940dbd7e5492
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @array.h:124:5@

__exported by:__ @array.h@
-}
fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3e6c940dbd7e5492

foreign import ccall unsafe "hs_bindgen_test_array_d9f87d3e541b15e5" hs_bindgen_test_array_d9f87d3e541b15e5
  :: IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @array.h:127:5@

__exported by:__ @array.h@
-}
fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_d9f87d3e541b15e5

foreign import ccall unsafe "hs_bindgen_test_array_cd41e41992d89300" hs_bindgen_test_array_cd41e41992d89300
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @array.h:130:5@

__exported by:__ @array.h@
-}
fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_cd41e41992d89300

foreign import ccall unsafe "hs_bindgen_test_array_db0e2655437ab8bb" hs_bindgen_test_array_db0e2655437ab8bb
  :: IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @array.h:133:5@

__exported by:__ @array.h@
-}
fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_db0e2655437ab8bb

foreign import ccall unsafe "hs_bindgen_test_array_9ec02aa16b020aa0" hs_bindgen_test_array_9ec02aa16b020aa0
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @array.h:136:5@

__exported by:__ @array.h@
-}
fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9ec02aa16b020aa0

foreign import ccall unsafe "hs_bindgen_test_array_a41b8d1332b69b95" hs_bindgen_test_array_a41b8d1332b69b95
  :: IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @array.h:139:5@

__exported by:__ @array.h@
-}
fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a41b8d1332b69b95

foreign import ccall unsafe "hs_bindgen_test_array_bdf2a6a8a3dd5b04" hs_bindgen_test_array_bdf2a6a8a3dd5b04
  :: IO (Ptr.FunPtr (Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @array.h:142:5@

__exported by:__ @array.h@
-}
isSolved_ptr :: Ptr.FunPtr (Sudoku -> IO FC.CInt)
isSolved_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_bdf2a6a8a3dd5b04

foreign import ccall unsafe "hs_bindgen_test_array_a3de5f7e233ad0e1" hs_bindgen_test_array_a3de5f7e233ad0e1
  :: IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @array.h:149:5@

__exported by:__ @array.h@
-}
fun_1_const_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a3de5f7e233ad0e1

foreign import ccall unsafe "hs_bindgen_test_array_3c09bbba7534ca1d" hs_bindgen_test_array_3c09bbba7534ca1d
  :: IO (Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @array.h:152:5@

__exported by:__ @array.h@
-}
fun_2_const_ptr :: Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt)
fun_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3c09bbba7534ca1d

foreign import ccall unsafe "hs_bindgen_test_array_0e53ed28ec1ca276" hs_bindgen_test_array_0e53ed28ec1ca276
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @array.h:155:5@

__exported by:__ @array.h@
-}
fun_3_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_0e53ed28ec1ca276

foreign import ccall unsafe "hs_bindgen_test_array_07d860d5e74c415b" hs_bindgen_test_array_07d860d5e74c415b
  :: IO (Ptr.FunPtr (List -> List -> IO FC.CInt))

{-# NOINLINE fun_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @array.h:158:5@

__exported by:__ @array.h@
-}
fun_4_const_ptr :: Ptr.FunPtr (List -> List -> IO FC.CInt)
fun_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_07d860d5e74c415b

foreign import ccall unsafe "hs_bindgen_test_array_3c0a139c24d7202a" hs_bindgen_test_array_3c0a139c24d7202a
  :: IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @array.h:161:5@

__exported by:__ @array.h@
-}
fun_5_const_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_3c0a139c24d7202a

foreign import ccall unsafe "hs_bindgen_test_array_62d236581cc18366" hs_bindgen_test_array_62d236581cc18366
  :: IO (Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @array.h:164:5@

__exported by:__ @array.h@
-}
fun_6_const_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt)
fun_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_62d236581cc18366

foreign import ccall unsafe "hs_bindgen_test_array_b4bf67c3cec12e54" hs_bindgen_test_array_b4bf67c3cec12e54
  :: IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @array.h:167:5@

__exported by:__ @array.h@
-}
fun_7_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_b4bf67c3cec12e54

foreign import ccall unsafe "hs_bindgen_test_array_99dd6a6017eb0eec" hs_bindgen_test_array_99dd6a6017eb0eec
  :: IO (Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @array.h:170:5@

__exported by:__ @array.h@
-}
fun_8_const_ptr :: Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt)
fun_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_99dd6a6017eb0eec

foreign import ccall unsafe "hs_bindgen_test_array_6deec046c95e4e0d" hs_bindgen_test_array_6deec046c95e4e0d
  :: IO (Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_const_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @array.h:173:5@

__exported by:__ @array.h@
-}
isSolved_const_ptr :: Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt)
isSolved_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_6deec046c95e4e0d

foreign import ccall unsafe "hs_bindgen_test_array_76f53f330102e743" hs_bindgen_test_array_76f53f330102e743
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9_ptr #-}

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @array.h:185:7@

__exported by:__ @array.h@
-}
fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_76f53f330102e743

foreign import ccall unsafe "hs_bindgen_test_array_abcc94f01de77b25" hs_bindgen_test_array_abcc94f01de77b25
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @array.h:188:10@

__exported by:__ @array.h@
-}
fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_abcc94f01de77b25

foreign import ccall unsafe "hs_bindgen_test_array_6661b46e4a751a85" hs_bindgen_test_array_6661b46e4a751a85
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @array.h:191:7@

__exported by:__ @array.h@
-}
fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_6661b46e4a751a85

foreign import ccall unsafe "hs_bindgen_test_array_9c80a9e3300aad15" hs_bindgen_test_array_9c80a9e3300aad15
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @array.h:194:7@

__exported by:__ @array.h@
-}
fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9c80a9e3300aad15

foreign import ccall unsafe "hs_bindgen_test_array_bb741b7e8c029e7e" hs_bindgen_test_array_bb741b7e8c029e7e
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @array.h:197:7@

__exported by:__ @array.h@
-}
fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_bb741b7e8c029e7e

foreign import ccall unsafe "hs_bindgen_test_array_75d83252a55a5c64" hs_bindgen_test_array_75d83252a55a5c64
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @array.h:200:9@

__exported by:__ @array.h@
-}
fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_75d83252a55a5c64

foreign import ccall unsafe "hs_bindgen_test_array_069ac2d1873f3210" hs_bindgen_test_array_069ac2d1873f3210
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @array.h:203:7@

__exported by:__ @array.h@
-}
fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_069ac2d1873f3210

foreign import ccall unsafe "hs_bindgen_test_array_314971335aaa6db3" hs_bindgen_test_array_314971335aaa6db3
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @array.h:206:14@

__exported by:__ @array.h@
-}
fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_314971335aaa6db3

foreign import ccall unsafe "hs_bindgen_test_array_9a62b5848be64bd4" hs_bindgen_test_array_9a62b5848be64bd4
  :: IO (Ptr.FunPtr (IO (Ptr.Ptr Sudoku)))

{-# NOINLINE solve_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @array.h:209:10@

__exported by:__ @array.h@
-}
solve_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Sudoku))
solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_9a62b5848be64bd4

foreign import ccall unsafe "hs_bindgen_test_array_a6413f4d2092265d" hs_bindgen_test_array_a6413f4d2092265d
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr0_ptr #-}

{-| Global, complete, not initialised

__C declaration:__ @arr0@

__defined at:__ @array.h:11:5@

__exported by:__ @array.h@
-}
arr0_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr0_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a6413f4d2092265d

foreign import ccall unsafe "hs_bindgen_test_array_1693226264ba4aeb" hs_bindgen_test_array_1693226264ba4aeb
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr1_ptr #-}

{-| Global, complete, initialised

__C declaration:__ @arr1@

__defined at:__ @array.h:14:5@

__exported by:__ @array.h@
-}
arr1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1693226264ba4aeb

foreign import ccall unsafe "hs_bindgen_test_array_dafcf99a73b93389" hs_bindgen_test_array_dafcf99a73b93389
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr2_ptr #-}

{-| Global, extern, complete, not initialised

__C declaration:__ @arr2@

__defined at:__ @array.h:17:12@

__exported by:__ @array.h@
-}
arr2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_dafcf99a73b93389

foreign import ccall unsafe "hs_bindgen_test_array_ca1016acc3449dee" hs_bindgen_test_array_ca1016acc3449dee
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr3_ptr #-}

{-| Global, extern, complete, initialised

__C declaration:__ @arr3@

__defined at:__ @array.h:20:12@

__exported by:__ @array.h@
-}
arr3_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_ca1016acc3449dee

foreign import ccall unsafe "hs_bindgen_test_array_1a8c921160bc99a6" hs_bindgen_test_array_1a8c921160bc99a6
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt))

{-# NOINLINE arr6_ptr #-}

{-| Global, incomplete

__C declaration:__ @arr6@

__defined at:__ @array.h:29:5@

__exported by:__ @array.h@
-}
arr6_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)
arr6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1a8c921160bc99a6

foreign import ccall unsafe "hs_bindgen_test_array_17cf970243739b65" hs_bindgen_test_array_17cf970243739b65
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr7_ptr #-}

{-| Global, extern, incomplete

__C declaration:__ @arr7@

__defined at:__ @array.h:32:12@

__exported by:__ @array.h@
-}
arr7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_17cf970243739b65

foreign import ccall unsafe "hs_bindgen_test_array_85bc33b188037456" hs_bindgen_test_array_85bc33b188037456
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1@

__defined at:__ @array.h:62:12@

__exported by:__ @array.h@
-}
arr_1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_85bc33b188037456

foreign import ccall unsafe "hs_bindgen_test_array_87c784150cd3ff65" hs_bindgen_test_array_87c784150cd3ff65
  :: IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2@

__defined at:__ @array.h:65:16@

__exported by:__ @array.h@
-}
arr_2_ptr :: Ptr.Ptr Triplet
arr_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_87c784150cd3ff65

foreign import ccall unsafe "hs_bindgen_test_array_e7b0de7633a7a62a" hs_bindgen_test_array_e7b0de7633a7a62a
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3@

__defined at:__ @array.h:68:12@

__exported by:__ @array.h@
-}
arr_3_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_e7b0de7633a7a62a

foreign import ccall unsafe "hs_bindgen_test_array_8fb64bc6c2bd4c73" hs_bindgen_test_array_8fb64bc6c2bd4c73
  :: IO (Ptr.Ptr List)

{-# NOINLINE arr_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4@

__defined at:__ @array.h:71:13@

__exported by:__ @array.h@
-}
arr_4_ptr :: Ptr.Ptr List
arr_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_8fb64bc6c2bd4c73

foreign import ccall unsafe "hs_bindgen_test_array_7348a94e6adce96e" hs_bindgen_test_array_7348a94e6adce96e
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5@

__defined at:__ @array.h:74:12@

__exported by:__ @array.h@
-}
arr_5_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_7348a94e6adce96e

foreign import ccall unsafe "hs_bindgen_test_array_1308613140bb4b80" hs_bindgen_test_array_1308613140bb4b80
  :: IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6@

__defined at:__ @array.h:77:15@

__exported by:__ @array.h@
-}
arr_6_ptr :: Ptr.Ptr Matrix
arr_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_1308613140bb4b80

foreign import ccall unsafe "hs_bindgen_test_array_a060984b378ed676" hs_bindgen_test_array_a060984b378ed676
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7@

__defined at:__ @array.h:80:12@

__exported by:__ @array.h@
-}
arr_7_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_a060984b378ed676

foreign import ccall unsafe "hs_bindgen_test_array_d82706abb6d8ea04" hs_bindgen_test_array_d82706abb6d8ea04
  :: IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8@

__defined at:__ @array.h:83:20@

__exported by:__ @array.h@
-}
arr_8_ptr :: Ptr.Ptr Tripletlist
arr_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_d82706abb6d8ea04

foreign import ccall unsafe "hs_bindgen_test_array_7376d172f5729493" hs_bindgen_test_array_7376d172f5729493
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-# NOINLINE arr_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @arr_1_const@

__defined at:__ @array.h:90:18@

__exported by:__ @array.h@
-}
arr_1_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
arr_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_7376d172f5729493

{-# NOINLINE arr_1_const #-}

arr_1_const :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
arr_1_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_1_const_ptr)

foreign import ccall unsafe "hs_bindgen_test_array_f03586aa57dfce29" hs_bindgen_test_array_f03586aa57dfce29
  :: IO (Ptr.Ptr Triplet)

{-# NOINLINE arr_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @arr_2_const@

__defined at:__ @array.h:93:22@

__exported by:__ @array.h@
-}
arr_2_const_ptr :: Ptr.Ptr Triplet
arr_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_f03586aa57dfce29

{-# NOINLINE arr_2_const #-}

arr_2_const :: Triplet
arr_2_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_2_const_ptr)

foreign import ccall unsafe "hs_bindgen_test_array_54ffd4ffcd2dad61" hs_bindgen_test_array_54ffd4ffcd2dad61
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-# NOINLINE arr_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @arr_3_const@

__defined at:__ @array.h:96:18@

__exported by:__ @array.h@
-}
arr_3_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
arr_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_54ffd4ffcd2dad61

foreign import ccall unsafe "hs_bindgen_test_array_8896c2ff5b9ce9c9" hs_bindgen_test_array_8896c2ff5b9ce9c9
  :: IO (Ptr.Ptr List)

{-# NOINLINE arr_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @arr_4_const@

__defined at:__ @array.h:99:19@

__exported by:__ @array.h@
-}
arr_4_const_ptr :: Ptr.Ptr List
arr_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_8896c2ff5b9ce9c9

foreign import ccall unsafe "hs_bindgen_test_array_46b406e096f6c9c1" hs_bindgen_test_array_46b406e096f6c9c1
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @arr_5_const@

__defined at:__ @array.h:102:18@

__exported by:__ @array.h@
-}
arr_5_const_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_46b406e096f6c9c1

foreign import ccall unsafe "hs_bindgen_test_array_ceb7f2027865ce12" hs_bindgen_test_array_ceb7f2027865ce12
  :: IO (Ptr.Ptr Matrix)

{-# NOINLINE arr_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @arr_6_const@

__defined at:__ @array.h:105:21@

__exported by:__ @array.h@
-}
arr_6_const_ptr :: Ptr.Ptr Matrix
arr_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_ceb7f2027865ce12

{-# NOINLINE arr_6_const #-}

arr_6_const :: Matrix
arr_6_const =
  GHC.IO.Unsafe.unsafePerformIO (F.peek arr_6_const_ptr)

foreign import ccall unsafe "hs_bindgen_test_array_2b565b2b97acdcb7" hs_bindgen_test_array_2b565b2b97acdcb7
  :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-# NOINLINE arr_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @arr_7_const@

__defined at:__ @array.h:108:18@

__exported by:__ @array.h@
-}
arr_7_const_ptr :: Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
arr_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_2b565b2b97acdcb7

foreign import ccall unsafe "hs_bindgen_test_array_03e2d9c4ef2ae993" hs_bindgen_test_array_03e2d9c4ef2ae993
  :: IO (Ptr.Ptr Tripletlist)

{-# NOINLINE arr_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @arr_8_const@

__defined at:__ @array.h:111:26@

__exported by:__ @array.h@
-}
arr_8_const_ptr :: Ptr.Ptr Tripletlist
arr_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_array_03e2d9c4ef2ae993
