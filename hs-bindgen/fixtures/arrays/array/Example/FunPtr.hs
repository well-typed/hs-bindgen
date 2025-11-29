{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr where

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
  , "/* get_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_3ced2f3b2af806f8 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3]"
  , ")"
  , "{"
  , "  return &fun_1;"
  , "}"
  , "/* get_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_84966994a8d7df93 (void)) ("
  , "  triplet arg1"
  , ")"
  , "{"
  , "  return &fun_2;"
  , "}"
  , "/* get_fun_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_3e6c940dbd7e5492 (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &fun_3;"
  , "}"
  , "/* get_fun_4_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_d9f87d3e541b15e5 (void)) ("
  , "  list arg1"
  , ")"
  , "{"
  , "  return &fun_4;"
  , "}"
  , "/* get_fun_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_cd41e41992d89300 (void)) ("
  , "  signed int arg1[4][3]"
  , ")"
  , "{"
  , "  return &fun_5;"
  , "}"
  , "/* get_fun_6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_db0e2655437ab8bb (void)) ("
  , "  matrix arg1"
  , ")"
  , "{"
  , "  return &fun_6;"
  , "}"
  , "/* get_fun_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_9ec02aa16b020aa0 (void)) ("
  , "  signed int arg1[][3]"
  , ")"
  , "{"
  , "  return &fun_7;"
  , "}"
  , "/* get_fun_8_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_a41b8d1332b69b95 (void)) ("
  , "  tripletlist arg1"
  , ")"
  , "{"
  , "  return &fun_8;"
  , "}"
  , "/* get_isSolved_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_bdf2a6a8a3dd5b04 (void)) ("
  , "  sudoku arg1"
  , ")"
  , "{"
  , "  return &isSolved;"
  , "}"
  , "/* get_fun_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_a3de5f7e233ad0e1 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3],"
  , "  signed int const arg3[3]"
  , ")"
  , "{"
  , "  return &fun_1_const;"
  , "}"
  , "/* get_fun_2_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_3c09bbba7534ca1d (void)) ("
  , "  triplet arg1,"
  , "  triplet const arg2"
  , ")"
  , "{"
  , "  return &fun_2_const;"
  , "}"
  , "/* get_fun_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_0e53ed28ec1ca276 (void)) ("
  , "  signed int arg1[],"
  , "  signed int const arg2[]"
  , ")"
  , "{"
  , "  return &fun_3_const;"
  , "}"
  , "/* get_fun_4_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_07d860d5e74c415b (void)) ("
  , "  list arg1,"
  , "  list const arg2"
  , ")"
  , "{"
  , "  return &fun_4_const;"
  , "}"
  , "/* get_fun_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_3c0a139c24d7202a (void)) ("
  , "  signed int arg1[4][3],"
  , "  signed int const arg2[4][3]"
  , ")"
  , "{"
  , "  return &fun_5_const;"
  , "}"
  , "/* get_fun_6_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_62d236581cc18366 (void)) ("
  , "  matrix arg1,"
  , "  matrix const arg2"
  , ")"
  , "{"
  , "  return &fun_6_const;"
  , "}"
  , "/* get_fun_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_b4bf67c3cec12e54 (void)) ("
  , "  signed int arg1[][3],"
  , "  signed int const arg2[][3]"
  , ")"
  , "{"
  , "  return &fun_7_const;"
  , "}"
  , "/* get_fun_8_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_99dd6a6017eb0eec (void)) ("
  , "  tripletlist arg1,"
  , "  tripletlist const arg2"
  , ")"
  , "{"
  , "  return &fun_8_const;"
  , "}"
  , "/* get_isSolved_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_6deec046c95e4e0d (void)) ("
  , "  sudoku arg1,"
  , "  sudoku const arg2"
  , ")"
  , "{"
  , "  return &isSolved_const;"
  , "}"
  , "/* get_fun_9_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_76f53f330102e743 (void)) (void))[3]"
  , "{"
  , "  return &fun_9;"
  , "}"
  , "/* get_fun_10_ptr */"
  , "__attribute__ ((const))"
  , "triplet *(*hs_bindgen_test_arraysarray_abcc94f01de77b25 (void)) (void)"
  , "{"
  , "  return &fun_10;"
  , "}"
  , "/* get_fun_11_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_6661b46e4a751a85 (void)) (void))[]"
  , "{"
  , "  return &fun_11;"
  , "}"
  , "/* get_fun_12_ptr */"
  , "__attribute__ ((const))"
  , "list *(*hs_bindgen_test_arraysarray_9c80a9e3300aad15 (void)) (void)"
  , "{"
  , "  return &fun_12;"
  , "}"
  , "/* get_fun_13_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_bb741b7e8c029e7e (void)) (void))[4][3]"
  , "{"
  , "  return &fun_13;"
  , "}"
  , "/* get_fun_14_ptr */"
  , "__attribute__ ((const))"
  , "matrix *(*hs_bindgen_test_arraysarray_75d83252a55a5c64 (void)) (void)"
  , "{"
  , "  return &fun_14;"
  , "}"
  , "/* get_fun_15_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_069ac2d1873f3210 (void)) (void))[][3]"
  , "{"
  , "  return &fun_15;"
  , "}"
  , "/* get_fun_16_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *(*hs_bindgen_test_arraysarray_314971335aaa6db3 (void)) (void)"
  , "{"
  , "  return &fun_16;"
  , "}"
  , "/* get_solve_ptr */"
  , "__attribute__ ((const))"
  , "sudoku *(*hs_bindgen_test_arraysarray_9a62b5848be64bd4 (void)) (void)"
  , "{"
  , "  return &solve;"
  , "}"
  ]))

{-| __unique:__ @ExampleNothingget_fun_1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_3ced2f3b2af806f8" hs_bindgen_test_arraysarray_3ced2f3b2af806f8 ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_3ced2f3b2af806f8

{-| __unique:__ @ExampleNothingget_fun_2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_84966994a8d7df93" hs_bindgen_test_arraysarray_84966994a8d7df93 ::
     IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_84966994a8d7df93

{-| __unique:__ @ExampleNothingget_fun_3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_3e6c940dbd7e5492" hs_bindgen_test_arraysarray_3e6c940dbd7e5492 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_3e6c940dbd7e5492

{-| __unique:__ @ExampleNothingget_fun_4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d9f87d3e541b15e5" hs_bindgen_test_arraysarray_d9f87d3e541b15e5 ::
     IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d9f87d3e541b15e5

{-| __unique:__ @ExampleNothingget_fun_5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_cd41e41992d89300" hs_bindgen_test_arraysarray_cd41e41992d89300 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_cd41e41992d89300

{-| __unique:__ @ExampleNothingget_fun_6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_db0e2655437ab8bb" hs_bindgen_test_arraysarray_db0e2655437ab8bb ::
     IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_db0e2655437ab8bb

{-| __unique:__ @ExampleNothingget_fun_7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9ec02aa16b020aa0" hs_bindgen_test_arraysarray_9ec02aa16b020aa0 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9ec02aa16b020aa0

{-| __unique:__ @ExampleNothingget_fun_8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a41b8d1332b69b95" hs_bindgen_test_arraysarray_a41b8d1332b69b95 ::
     IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a41b8d1332b69b95

{-| __unique:__ @ExampleNothingget_isSolved_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_bdf2a6a8a3dd5b04" hs_bindgen_test_arraysarray_bdf2a6a8a3dd5b04 ::
     IO (Ptr.FunPtr (Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_ptr :: Ptr.FunPtr (Sudoku -> IO FC.CInt)
isSolved_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_bdf2a6a8a3dd5b04

{-| __unique:__ @ExampleNothingget_fun_1_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_a3de5f7e233ad0e1" hs_bindgen_test_arraysarray_a3de5f7e233ad0e1 ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_a3de5f7e233ad0e1

{-| __unique:__ @ExampleNothingget_fun_2_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_3c09bbba7534ca1d" hs_bindgen_test_arraysarray_3c09bbba7534ca1d ::
     IO (Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const_ptr :: Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt)
fun_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_3c09bbba7534ca1d

{-| __unique:__ @ExampleNothingget_fun_3_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_0e53ed28ec1ca276" hs_bindgen_test_arraysarray_0e53ed28ec1ca276 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_0e53ed28ec1ca276

{-| __unique:__ @ExampleNothingget_fun_4_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_07d860d5e74c415b" hs_bindgen_test_arraysarray_07d860d5e74c415b ::
     IO (Ptr.FunPtr (List -> List -> IO FC.CInt))

{-# NOINLINE fun_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const_ptr :: Ptr.FunPtr (List -> List -> IO FC.CInt)
fun_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_07d860d5e74c415b

{-| __unique:__ @ExampleNothingget_fun_5_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_3c0a139c24d7202a" hs_bindgen_test_arraysarray_3c0a139c24d7202a ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_3c0a139c24d7202a

{-| __unique:__ @ExampleNothingget_fun_6_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_62d236581cc18366" hs_bindgen_test_arraysarray_62d236581cc18366 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt)
fun_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_62d236581cc18366

{-| __unique:__ @ExampleNothingget_fun_7_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_b4bf67c3cec12e54" hs_bindgen_test_arraysarray_b4bf67c3cec12e54 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_b4bf67c3cec12e54

{-| __unique:__ @ExampleNothingget_fun_8_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_99dd6a6017eb0eec" hs_bindgen_test_arraysarray_99dd6a6017eb0eec ::
     IO (Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const_ptr :: Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt)
fun_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_99dd6a6017eb0eec

{-| __unique:__ @ExampleNothingget_isSolved_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6deec046c95e4e0d" hs_bindgen_test_arraysarray_6deec046c95e4e0d ::
     IO (Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_const_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const_ptr :: Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt)
isSolved_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6deec046c95e4e0d

{-| __unique:__ @ExampleNothingget_fun_9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_76f53f330102e743" hs_bindgen_test_arraysarray_76f53f330102e743 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9_ptr #-}

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_76f53f330102e743

{-| __unique:__ @ExampleNothingget_fun_10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_abcc94f01de77b25" hs_bindgen_test_arraysarray_abcc94f01de77b25 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_abcc94f01de77b25

{-| __unique:__ @ExampleNothingget_fun_11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6661b46e4a751a85" hs_bindgen_test_arraysarray_6661b46e4a751a85 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6661b46e4a751a85

{-| __unique:__ @ExampleNothingget_fun_12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9c80a9e3300aad15" hs_bindgen_test_arraysarray_9c80a9e3300aad15 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9c80a9e3300aad15

{-| __unique:__ @ExampleNothingget_fun_13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_bb741b7e8c029e7e" hs_bindgen_test_arraysarray_bb741b7e8c029e7e ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_bb741b7e8c029e7e

{-| __unique:__ @ExampleNothingget_fun_14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_75d83252a55a5c64" hs_bindgen_test_arraysarray_75d83252a55a5c64 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_75d83252a55a5c64

{-| __unique:__ @ExampleNothingget_fun_15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_069ac2d1873f3210" hs_bindgen_test_arraysarray_069ac2d1873f3210 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_069ac2d1873f3210

{-| __unique:__ @ExampleNothingget_fun_16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_314971335aaa6db3" hs_bindgen_test_arraysarray_314971335aaa6db3 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_314971335aaa6db3

{-| __unique:__ @ExampleNothingget_solve_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9a62b5848be64bd4" hs_bindgen_test_arraysarray_9a62b5848be64bd4 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Sudoku)))

{-# NOINLINE solve_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Sudoku))
solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9a62b5848be64bd4
