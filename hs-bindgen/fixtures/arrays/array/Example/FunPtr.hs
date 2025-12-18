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
  , "/* test_arraysarray_Example_get_fun_1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5c9712c1e2ca2187 (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3]"
  , ")"
  , "{"
  , "  return &fun_1;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_215c9a69aea5166e (void)) ("
  , "  triplet arg1"
  , ")"
  , "{"
  , "  return &fun_2;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d45ffb769078021d (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &fun_3;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_16f300d083285e38 (void)) ("
  , "  list arg1"
  , ")"
  , "{"
  , "  return &fun_4;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_05ddc0274405d6f5 (void)) ("
  , "  signed int arg1[4][3]"
  , ")"
  , "{"
  , "  return &fun_5;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bc6a8e5360af8799 (void)) ("
  , "  matrix arg1"
  , ")"
  , "{"
  , "  return &fun_6;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b633a17af8e60afd (void)) ("
  , "  signed int arg1[][3]"
  , ")"
  , "{"
  , "  return &fun_7;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0801bc85e75e7232 (void)) ("
  , "  tripletlist arg1"
  , ")"
  , "{"
  , "  return &fun_8;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f525d0058f613e21 (void)) ("
  , "  sudoku arg1"
  , ")"
  , "{"
  , "  return &isSolved;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_1_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0fdde9c360622daf (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3],"
  , "  signed int const arg3[3]"
  , ")"
  , "{"
  , "  return &fun_1_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8182488430e66fdc (void)) ("
  , "  triplet arg1,"
  , "  triplet const arg2"
  , ")"
  , "{"
  , "  return &fun_2_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2303f5a2f855a6e5 (void)) ("
  , "  signed int arg1[],"
  , "  signed int const arg2[]"
  , ")"
  , "{"
  , "  return &fun_3_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4570b26d96819cd9 (void)) ("
  , "  list arg1,"
  , "  list const arg2"
  , ")"
  , "{"
  , "  return &fun_4_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_65a5e31cfb885928 (void)) ("
  , "  signed int arg1[4][3],"
  , "  signed int const arg2[4][3]"
  , ")"
  , "{"
  , "  return &fun_5_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_107e8456ef4df268 (void)) ("
  , "  matrix arg1,"
  , "  matrix const arg2"
  , ")"
  , "{"
  , "  return &fun_6_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_249a199499d81ca8 (void)) ("
  , "  signed int arg1[][3],"
  , "  signed int const arg2[][3]"
  , ")"
  , "{"
  , "  return &fun_7_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_92bc0bfe006ce53c (void)) ("
  , "  tripletlist arg1,"
  , "  tripletlist const arg2"
  , ")"
  , "{"
  , "  return &fun_8_const;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_935428cb3e78f9a5 (void)) ("
  , "  sudoku arg1,"
  , "  sudoku const arg2"
  , ")"
  , "{"
  , "  return &isSolved_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_9 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_c9bec6ab325b063c (void)) (void))[3]"
  , "{"
  , "  return &fun_9;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_10 */"
  , "__attribute__ ((const))"
  , "triplet *(*hs_bindgen_49f85263b6310047 (void)) (void)"
  , "{"
  , "  return &fun_10;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_11 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_68bdd4a6e02a046b (void)) (void))[]"
  , "{"
  , "  return &fun_11;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_12 */"
  , "__attribute__ ((const))"
  , "list *(*hs_bindgen_0521f5208260a928 (void)) (void)"
  , "{"
  , "  return &fun_12;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_13 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_9d0f2421c940811c (void)) (void))[4][3]"
  , "{"
  , "  return &fun_13;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_14 */"
  , "__attribute__ ((const))"
  , "matrix *(*hs_bindgen_50eeeb460ce92a1d (void)) (void)"
  , "{"
  , "  return &fun_14;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_15 */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_afb15d11d3db71eb (void)) (void))[][3]"
  , "{"
  , "  return &fun_15;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_16 */"
  , "__attribute__ ((const))"
  , "tripletlist *(*hs_bindgen_f41fbdca0717f85b (void)) (void)"
  , "{"
  , "  return &fun_16;"
  , "}"
  , "/* test_arraysarray_Example_get_solve */"
  , "__attribute__ ((const))"
  , "sudoku *(*hs_bindgen_b219669884cf1eb6 (void)) (void)"
  , "{"
  , "  return &solve;"
  , "}"
  ]))

-- | __unique:__ @test_arraysarray_Example_get_fun_1@
foreign import ccall unsafe "hs_bindgen_5c9712c1e2ca2187" hs_bindgen_5c9712c1e2ca2187 ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1 #-}

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1 :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5c9712c1e2ca2187

-- | __unique:__ @test_arraysarray_Example_get_fun_2@
foreign import ccall unsafe "hs_bindgen_215c9a69aea5166e" hs_bindgen_215c9a69aea5166e ::
     IO (Ptr.FunPtr (Triplet -> IO FC.CInt))

{-# NOINLINE fun_2 #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_215c9a69aea5166e

-- | __unique:__ @test_arraysarray_Example_get_fun_3@
foreign import ccall unsafe "hs_bindgen_d45ffb769078021d" hs_bindgen_d45ffb769078021d ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3 #-}

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d45ffb769078021d

-- | __unique:__ @test_arraysarray_Example_get_fun_4@
foreign import ccall unsafe "hs_bindgen_16f300d083285e38" hs_bindgen_16f300d083285e38 ::
     IO (Ptr.FunPtr (List -> IO FC.CInt))

{-# NOINLINE fun_4 #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_16f300d083285e38

-- | __unique:__ @test_arraysarray_Example_get_fun_5@
foreign import ccall unsafe "hs_bindgen_05ddc0274405d6f5" hs_bindgen_05ddc0274405d6f5 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5 #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_05ddc0274405d6f5

-- | __unique:__ @test_arraysarray_Example_get_fun_6@
foreign import ccall unsafe "hs_bindgen_bc6a8e5360af8799" hs_bindgen_bc6a8e5360af8799 ::
     IO (Ptr.FunPtr (Matrix -> IO FC.CInt))

{-# NOINLINE fun_6 #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bc6a8e5360af8799

-- | __unique:__ @test_arraysarray_Example_get_fun_7@
foreign import ccall unsafe "hs_bindgen_b633a17af8e60afd" hs_bindgen_b633a17af8e60afd ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7 #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b633a17af8e60afd

-- | __unique:__ @test_arraysarray_Example_get_fun_8@
foreign import ccall unsafe "hs_bindgen_0801bc85e75e7232" hs_bindgen_0801bc85e75e7232 ::
     IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8 #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0801bc85e75e7232

-- | __unique:__ @test_arraysarray_Example_get_isSolved@
foreign import ccall unsafe "hs_bindgen_f525d0058f613e21" hs_bindgen_f525d0058f613e21 ::
     IO (Ptr.FunPtr (Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved :: Ptr.FunPtr (Sudoku -> IO FC.CInt)
isSolved =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f525d0058f613e21

-- | __unique:__ @test_arraysarray_Example_get_fun_1_const@
foreign import ccall unsafe "hs_bindgen_0fdde9c360622daf" hs_bindgen_0fdde9c360622daf ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_1_const #-}

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0fdde9c360622daf

-- | __unique:__ @test_arraysarray_Example_get_fun_2_const@
foreign import ccall unsafe "hs_bindgen_8182488430e66fdc" hs_bindgen_8182488430e66fdc ::
     IO (Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt))

{-# NOINLINE fun_2_const #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const :: Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt)
fun_2_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8182488430e66fdc

-- | __unique:__ @test_arraysarray_Example_get_fun_3_const@
foreign import ccall unsafe "hs_bindgen_2303f5a2f855a6e5" hs_bindgen_2303f5a2f855a6e5 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))

{-# NOINLINE fun_3_const #-}

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2303f5a2f855a6e5

-- | __unique:__ @test_arraysarray_Example_get_fun_4_const@
foreign import ccall unsafe "hs_bindgen_4570b26d96819cd9" hs_bindgen_4570b26d96819cd9 ::
     IO (Ptr.FunPtr (List -> List -> IO FC.CInt))

{-# NOINLINE fun_4_const #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const :: Ptr.FunPtr (List -> List -> IO FC.CInt)
fun_4_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4570b26d96819cd9

-- | __unique:__ @test_arraysarray_Example_get_fun_5_const@
foreign import ccall unsafe "hs_bindgen_65a5e31cfb885928" hs_bindgen_65a5e31cfb885928 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_5_const #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_65a5e31cfb885928

-- | __unique:__ @test_arraysarray_Example_get_fun_6_const@
foreign import ccall unsafe "hs_bindgen_107e8456ef4df268" hs_bindgen_107e8456ef4df268 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt))

{-# NOINLINE fun_6_const #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const :: Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt)
fun_6_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_107e8456ef4df268

-- | __unique:__ @test_arraysarray_Example_get_fun_7_const@
foreign import ccall unsafe "hs_bindgen_249a199499d81ca8" hs_bindgen_249a199499d81ca8 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))

{-# NOINLINE fun_7_const #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_249a199499d81ca8

-- | __unique:__ @test_arraysarray_Example_get_fun_8_const@
foreign import ccall unsafe "hs_bindgen_92bc0bfe006ce53c" hs_bindgen_92bc0bfe006ce53c ::
     IO (Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt))

{-# NOINLINE fun_8_const #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const :: Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt)
fun_8_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_92bc0bfe006ce53c

-- | __unique:__ @test_arraysarray_Example_get_isSolved_const@
foreign import ccall unsafe "hs_bindgen_935428cb3e78f9a5" hs_bindgen_935428cb3e78f9a5 ::
     IO (Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt))

{-# NOINLINE isSolved_const #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const :: Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt)
isSolved_const =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_935428cb3e78f9a5

-- | __unique:__ @test_arraysarray_Example_get_fun_9@
foreign import ccall unsafe "hs_bindgen_c9bec6ab325b063c" hs_bindgen_c9bec6ab325b063c ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))

{-# NOINLINE fun_9 #-}

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c9bec6ab325b063c

-- | __unique:__ @test_arraysarray_Example_get_fun_10@
foreign import ccall unsafe "hs_bindgen_49f85263b6310047" hs_bindgen_49f85263b6310047 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))

{-# NOINLINE fun_10 #-}

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_49f85263b6310047

-- | __unique:__ @test_arraysarray_Example_get_fun_11@
foreign import ccall unsafe "hs_bindgen_68bdd4a6e02a046b" hs_bindgen_68bdd4a6e02a046b ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))

{-# NOINLINE fun_11 #-}

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_68bdd4a6e02a046b

-- | __unique:__ @test_arraysarray_Example_get_fun_12@
foreign import ccall unsafe "hs_bindgen_0521f5208260a928" hs_bindgen_0521f5208260a928 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr List)))

{-# NOINLINE fun_12 #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0521f5208260a928

-- | __unique:__ @test_arraysarray_Example_get_fun_13@
foreign import ccall unsafe "hs_bindgen_9d0f2421c940811c" hs_bindgen_9d0f2421c940811c ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_13 #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9d0f2421c940811c

-- | __unique:__ @test_arraysarray_Example_get_fun_14@
foreign import ccall unsafe "hs_bindgen_50eeeb460ce92a1d" hs_bindgen_50eeeb460ce92a1d ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))

{-# NOINLINE fun_14 #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_50eeeb460ce92a1d

-- | __unique:__ @test_arraysarray_Example_get_fun_15@
foreign import ccall unsafe "hs_bindgen_afb15d11d3db71eb" hs_bindgen_afb15d11d3db71eb ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))

{-# NOINLINE fun_15 #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afb15d11d3db71eb

-- | __unique:__ @test_arraysarray_Example_get_fun_16@
foreign import ccall unsafe "hs_bindgen_f41fbdca0717f85b" hs_bindgen_f41fbdca0717f85b ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))

{-# NOINLINE fun_16 #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f41fbdca0717f85b

-- | __unique:__ @test_arraysarray_Example_get_solve@
foreign import ccall unsafe "hs_bindgen_b219669884cf1eb6" hs_bindgen_b219669884cf1eb6 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Sudoku)))

{-# NOINLINE solve #-}

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve :: Ptr.FunPtr (IO (Ptr.Ptr Sudoku))
solve =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b219669884cf1eb6
