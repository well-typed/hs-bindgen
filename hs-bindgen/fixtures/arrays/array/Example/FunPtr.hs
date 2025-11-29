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
  , "/* Example_get_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_6b6e32833809f4de (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3]"
  , ")"
  , "{"
  , "  return &fun_1;"
  , "}"
  , "/* Example_get_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_5096fa9b8667e662 (void)) ("
  , "  triplet arg1"
  , ")"
  , "{"
  , "  return &fun_2;"
  , "}"
  , "/* Example_get_fun_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_3f5a59e00d7447b1 (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &fun_3;"
  , "}"
  , "/* Example_get_fun_4_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_d3dc3ffdf08324e8 (void)) ("
  , "  list arg1"
  , ")"
  , "{"
  , "  return &fun_4;"
  , "}"
  , "/* Example_get_fun_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_20bdd029d1f92402 (void)) ("
  , "  signed int arg1[4][3]"
  , ")"
  , "{"
  , "  return &fun_5;"
  , "}"
  , "/* Example_get_fun_6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_9dc31a73e785e428 (void)) ("
  , "  matrix arg1"
  , ")"
  , "{"
  , "  return &fun_6;"
  , "}"
  , "/* Example_get_fun_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_56d34538885823fc (void)) ("
  , "  signed int arg1[][3]"
  , ")"
  , "{"
  , "  return &fun_7;"
  , "}"
  , "/* Example_get_fun_8_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_c987b0a05ff1a4f9 (void)) ("
  , "  tripletlist arg1"
  , ")"
  , "{"
  , "  return &fun_8;"
  , "}"
  , "/* Example_get_isSolved_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_5c14ca7cbade870e (void)) ("
  , "  sudoku arg1"
  , ")"
  , "{"
  , "  return &isSolved;"
  , "}"
  , "/* Example_get_fun_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_9884694f1c40fda3 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3],"
  , "  signed int const arg3[3]"
  , ")"
  , "{"
  , "  return &fun_1_const;"
  , "}"
  , "/* Example_get_fun_2_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_e9eb1d57f25c21ae (void)) ("
  , "  triplet arg1,"
  , "  triplet const arg2"
  , ")"
  , "{"
  , "  return &fun_2_const;"
  , "}"
  , "/* Example_get_fun_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_735a91c04493a982 (void)) ("
  , "  signed int arg1[],"
  , "  signed int const arg2[]"
  , ")"
  , "{"
  , "  return &fun_3_const;"
  , "}"
  , "/* Example_get_fun_4_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_fea20c273516e1fe (void)) ("
  , "  list arg1,"
  , "  list const arg2"
  , ")"
  , "{"
  , "  return &fun_4_const;"
  , "}"
  , "/* Example_get_fun_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_e11fbfe76b31903c (void)) ("
  , "  signed int arg1[4][3],"
  , "  signed int const arg2[4][3]"
  , ")"
  , "{"
  , "  return &fun_5_const;"
  , "}"
  , "/* Example_get_fun_6_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_c9ba3e1c272d4315 (void)) ("
  , "  matrix arg1,"
  , "  matrix const arg2"
  , ")"
  , "{"
  , "  return &fun_6_const;"
  , "}"
  , "/* Example_get_fun_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_1809c052a5e61d2a (void)) ("
  , "  signed int arg1[][3],"
  , "  signed int const arg2[][3]"
  , ")"
  , "{"
  , "  return &fun_7_const;"
  , "}"
  , "/* Example_get_fun_8_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_6b3c302ecb37220c (void)) ("
  , "  tripletlist arg1,"
  , "  tripletlist const arg2"
  , ")"
  , "{"
  , "  return &fun_8_const;"
  , "}"
  , "/* Example_get_isSolved_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_test_arraysarray_4b8528363ee2e7ca (void)) ("
  , "  sudoku arg1,"
  , "  sudoku const arg2"
  , ")"
  , "{"
  , "  return &isSolved_const;"
  , "}"
  , "/* Example_get_fun_9_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_52ff84979340538b (void)) (void))[3]"
  , "{"
  , "  return &fun_9;"
  , "}"
  , "/* Example_get_fun_10_ptr */"
  , "__attribute__ ((const))"
  , "triplet *(*hs_bindgen_test_arraysarray_5b7f6d7551d398b2 (void)) (void)"
  , "{"
  , "  return &fun_10;"
  , "}"
  , "/* Example_get_fun_11_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_eec099bedfc528a2 (void)) (void))[]"
  , "{"
  , "  return &fun_11;"
  , "}"
  , "/* Example_get_fun_12_ptr */"
  , "__attribute__ ((const))"
  , "list *(*hs_bindgen_test_arraysarray_2ff61673a0fdac05 (void)) (void)"
  , "{"
  , "  return &fun_12;"
  , "}"
  , "/* Example_get_fun_13_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_681d8a45f41b1b13 (void)) (void))[4][3]"
  , "{"
  , "  return &fun_13;"
  , "}"
  , "/* Example_get_fun_14_ptr */"
  , "__attribute__ ((const))"
  , "matrix *(*hs_bindgen_test_arraysarray_94a35955c5b3b3fc (void)) (void)"
  , "{"
  , "  return &fun_14;"
  , "}"
  , "/* Example_get_fun_15_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_test_arraysarray_ccb6620334c18511 (void)) (void))[][3]"
  , "{"
  , "  return &fun_15;"
  , "}"
  , "/* Example_get_fun_16_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *(*hs_bindgen_test_arraysarray_7d7dc77af9fa3a13 (void)) (void)"
  , "{"
  , "  return &fun_16;"
  , "}"
  , "/* Example_get_solve_ptr */"
  , "__attribute__ ((const))"
  , "sudoku *(*hs_bindgen_test_arraysarray_b2530e043784cab8 (void)) (void)"
  , "{"
  , "  return &solve;"
  , "}"
  ]))

{-| __unique:__ @Example_get_fun_1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6b6e32833809f4de" hs_bindgen_test_arraysarray_6b6e32833809f4de ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6b6e32833809f4de

{-| __unique:__ @Example_get_fun_2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_5096fa9b8667e662" hs_bindgen_test_arraysarray_5096fa9b8667e662 ::
     IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_5096fa9b8667e662

{-| __unique:__ @Example_get_fun_3_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_3f5a59e00d7447b1" hs_bindgen_test_arraysarray_3f5a59e00d7447b1 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_3f5a59e00d7447b1

{-| __unique:__ @Example_get_fun_4_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_d3dc3ffdf08324e8" hs_bindgen_test_arraysarray_d3dc3ffdf08324e8 ::
     IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_d3dc3ffdf08324e8

{-| __unique:__ @Example_get_fun_5_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_20bdd029d1f92402" hs_bindgen_test_arraysarray_20bdd029d1f92402 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_20bdd029d1f92402

{-| __unique:__ @Example_get_fun_6_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9dc31a73e785e428" hs_bindgen_test_arraysarray_9dc31a73e785e428 ::
     IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9dc31a73e785e428

{-| __unique:__ @Example_get_fun_7_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_56d34538885823fc" hs_bindgen_test_arraysarray_56d34538885823fc ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_56d34538885823fc

{-| __unique:__ @Example_get_fun_8_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_c987b0a05ff1a4f9" hs_bindgen_test_arraysarray_c987b0a05ff1a4f9 ::
     IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_c987b0a05ff1a4f9

{-| __unique:__ @Example_get_isSolved_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_5c14ca7cbade870e" hs_bindgen_test_arraysarray_5c14ca7cbade870e ::
     IO (Ptr.FunPtr (Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_ptr :: Ptr.FunPtr (Sudoku -> IO FC.CInt)
isSolved_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_5c14ca7cbade870e

{-| __unique:__ @Example_get_fun_1_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_9884694f1c40fda3" hs_bindgen_test_arraysarray_9884694f1c40fda3 ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_9884694f1c40fda3

{-| __unique:__ @Example_get_fun_2_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_e9eb1d57f25c21ae" hs_bindgen_test_arraysarray_e9eb1d57f25c21ae ::
     IO (Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const_ptr :: Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt)
fun_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_e9eb1d57f25c21ae

{-| __unique:__ @Example_get_fun_3_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_735a91c04493a982" hs_bindgen_test_arraysarray_735a91c04493a982 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_735a91c04493a982

{-| __unique:__ @Example_get_fun_4_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_fea20c273516e1fe" hs_bindgen_test_arraysarray_fea20c273516e1fe ::
     IO (Ptr.FunPtr (List -> List -> IO FC.CInt))

{-# NOINLINE fun_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const_ptr :: Ptr.FunPtr (List -> List -> IO FC.CInt)
fun_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_fea20c273516e1fe

{-| __unique:__ @Example_get_fun_5_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_e11fbfe76b31903c" hs_bindgen_test_arraysarray_e11fbfe76b31903c ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_e11fbfe76b31903c

{-| __unique:__ @Example_get_fun_6_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_c9ba3e1c272d4315" hs_bindgen_test_arraysarray_c9ba3e1c272d4315 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt)
fun_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_c9ba3e1c272d4315

{-| __unique:__ @Example_get_fun_7_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_1809c052a5e61d2a" hs_bindgen_test_arraysarray_1809c052a5e61d2a ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_1809c052a5e61d2a

{-| __unique:__ @Example_get_fun_8_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_6b3c302ecb37220c" hs_bindgen_test_arraysarray_6b3c302ecb37220c ::
     IO (Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const_ptr :: Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt)
fun_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_6b3c302ecb37220c

{-| __unique:__ @Example_get_isSolved_const_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_4b8528363ee2e7ca" hs_bindgen_test_arraysarray_4b8528363ee2e7ca ::
     IO (Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_const_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const_ptr :: Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt)
isSolved_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_4b8528363ee2e7ca

{-| __unique:__ @Example_get_fun_9_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_52ff84979340538b" hs_bindgen_test_arraysarray_52ff84979340538b ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9_ptr #-}

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_52ff84979340538b

{-| __unique:__ @Example_get_fun_10_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_5b7f6d7551d398b2" hs_bindgen_test_arraysarray_5b7f6d7551d398b2 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_5b7f6d7551d398b2

{-| __unique:__ @Example_get_fun_11_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_eec099bedfc528a2" hs_bindgen_test_arraysarray_eec099bedfc528a2 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_eec099bedfc528a2

{-| __unique:__ @Example_get_fun_12_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_2ff61673a0fdac05" hs_bindgen_test_arraysarray_2ff61673a0fdac05 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_2ff61673a0fdac05

{-| __unique:__ @Example_get_fun_13_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_681d8a45f41b1b13" hs_bindgen_test_arraysarray_681d8a45f41b1b13 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_681d8a45f41b1b13

{-| __unique:__ @Example_get_fun_14_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_94a35955c5b3b3fc" hs_bindgen_test_arraysarray_94a35955c5b3b3fc ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_94a35955c5b3b3fc

{-| __unique:__ @Example_get_fun_15_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_ccb6620334c18511" hs_bindgen_test_arraysarray_ccb6620334c18511 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_ccb6620334c18511

{-| __unique:__ @Example_get_fun_16_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_7d7dc77af9fa3a13" hs_bindgen_test_arraysarray_7d7dc77af9fa3a13 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_7d7dc77af9fa3a13

{-| __unique:__ @Example_get_solve_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_arraysarray_b2530e043784cab8" hs_bindgen_test_arraysarray_b2530e043784cab8 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Sudoku)))

{-# NOINLINE solve_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Sudoku))
solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_arraysarray_b2530e043784cab8
