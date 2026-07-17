{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.FunPtr
    ( Example.FunPtr.fun_1
    , Example.FunPtr.fun_2
    , Example.FunPtr.fun_3
    , Example.FunPtr.fun_4
    , Example.FunPtr.fun_5
    , Example.FunPtr.fun_6
    , Example.FunPtr.fun_7
    , Example.FunPtr.fun_8
    , Example.FunPtr.isSolved
    , Example.FunPtr.fun_1_const
    , Example.FunPtr.fun_2_const
    , Example.FunPtr.fun_3_const
    , Example.FunPtr.fun_4_const
    , Example.FunPtr.fun_5_const
    , Example.FunPtr.fun_6_const
    , Example.FunPtr.fun_7_const
    , Example.FunPtr.fun_8_const
    , Example.FunPtr.isSolved_const
    , Example.FunPtr.fun_9
    , Example.FunPtr.fun_10
    , Example.FunPtr.fun_11
    , Example.FunPtr.fun_12
    , Example.FunPtr.fun_13
    , Example.FunPtr.fun_14
    , Example.FunPtr.fun_15
    , Example.FunPtr.fun_16
    , Example.FunPtr.solve
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.IsArray as IsA
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
  [ "#include <arrays/array.h>"
  , "/* test_arraysarray_Example_get_fun_1 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_5c9712c1e2ca2187 (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return &fun_1;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_215c9a69aea5166e (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fun_2;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_d45ffb769078021d (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fun_3;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_16f300d083285e38 (void)) ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return &fun_4;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_05ddc0274405d6f5 (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &fun_5;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_bc6a8e5360af8799 (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &fun_6;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_b633a17af8e60afd (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &fun_7;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8 */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0801bc85e75e7232 (void)) ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return &fun_8;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_f525d0058f613e21 (void)) ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return &isSolved;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_1_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_0fdde9c360622daf (void)) ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const *arg3"
  , ")"
  , "{"
  , "  return &fun_1_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_8182488430e66fdc (void)) ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &fun_2_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2303f5a2f855a6e5 (void)) ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &fun_3_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4570b26d96819cd9 (void)) ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return &fun_4_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_65a5e31cfb885928 (void)) ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return &fun_5_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_107e8456ef4df268 (void)) ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return &fun_6_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_249a199499d81ca8 (void)) ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return &fun_7_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_92bc0bfe006ce53c (void)) ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return &fun_8_const;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved_const */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_935428cb3e78f9a5 (void)) ("
  , "  triplet *arg1,"
  , "  triplet const *arg2"
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

-- __unique:__ @test_arraysarray_Example_get_fun_1@
foreign import ccall unsafe "hs_bindgen_5c9712c1e2ca2187" hs_bindgen_5c9712c1e2ca2187_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_1@
hs_bindgen_5c9712c1e2ca2187 :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> IO BG.CInt))
hs_bindgen_5c9712c1e2ca2187 =
  BG.fromFFIType hs_bindgen_5c9712c1e2ca2187_base

{-# NOINLINE fun_1 #-}
{-| Array of known size.

    __C declaration:__ @fun_1@

    __defined at:__ @arrays\/array.h 118:5@

    __exported by:__ @arrays\/array.h@
-}
fun_1 :: BG.FunPtr (BG.CInt -> BG.Ptr (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> IO BG.CInt)
fun_1 =
  BG.unsafePerformIO hs_bindgen_5c9712c1e2ca2187

-- __unique:__ @test_arraysarray_Example_get_fun_2@
foreign import ccall unsafe "hs_bindgen_215c9a69aea5166e" hs_bindgen_215c9a69aea5166e_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_2@
hs_bindgen_215c9a69aea5166e :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Triplet) -> IO BG.CInt))
hs_bindgen_215c9a69aea5166e =
  BG.fromFFIType hs_bindgen_215c9a69aea5166e_base

{-# NOINLINE fun_2 #-}
{-| Array of known size, typedef.

    __C declaration:__ @fun_2@

    __defined at:__ @arrays\/array.h 121:5@

    __exported by:__ @arrays\/array.h@
-}
fun_2 :: BG.FunPtr (BG.Ptr (IsA.Elem Triplet) -> IO BG.CInt)
fun_2 =
  BG.unsafePerformIO hs_bindgen_215c9a69aea5166e

-- __unique:__ @test_arraysarray_Example_get_fun_3@
foreign import ccall unsafe "hs_bindgen_d45ffb769078021d" hs_bindgen_d45ffb769078021d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_3@
hs_bindgen_d45ffb769078021d :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO BG.CInt))
hs_bindgen_d45ffb769078021d =
  BG.fromFFIType hs_bindgen_d45ffb769078021d_base

{-# NOINLINE fun_3 #-}
{-| Array of unknown size.

    __C declaration:__ @fun_3@

    __defined at:__ @arrays\/array.h 124:5@

    __exported by:__ @arrays\/array.h@
-}
fun_3 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO BG.CInt)
fun_3 =
  BG.unsafePerformIO hs_bindgen_d45ffb769078021d

-- __unique:__ @test_arraysarray_Example_get_fun_4@
foreign import ccall unsafe "hs_bindgen_16f300d083285e38" hs_bindgen_16f300d083285e38_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_4@
hs_bindgen_16f300d083285e38 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem List) -> IO BG.CInt))
hs_bindgen_16f300d083285e38 =
  BG.fromFFIType hs_bindgen_16f300d083285e38_base

{-# NOINLINE fun_4 #-}
{-| Array of unknown size, typedef.

    __C declaration:__ @fun_4@

    __defined at:__ @arrays\/array.h 127:5@

    __exported by:__ @arrays\/array.h@
-}
fun_4 :: BG.FunPtr (BG.Ptr (IsA.Elem List) -> IO BG.CInt)
fun_4 =
  BG.unsafePerformIO hs_bindgen_16f300d083285e38

-- __unique:__ @test_arraysarray_Example_get_fun_5@
foreign import ccall unsafe "hs_bindgen_05ddc0274405d6f5" hs_bindgen_05ddc0274405d6f5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_5@
hs_bindgen_05ddc0274405d6f5 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt))
hs_bindgen_05ddc0274405d6f5 =
  BG.fromFFIType hs_bindgen_05ddc0274405d6f5_base

{-# NOINLINE fun_5 #-}
{-| Multi-dimensional array of known size.

    __C declaration:__ @fun_5@

    __defined at:__ @arrays\/array.h 130:5@

    __exported by:__ @arrays\/array.h@
-}
fun_5 :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt)
fun_5 =
  BG.unsafePerformIO hs_bindgen_05ddc0274405d6f5

-- __unique:__ @test_arraysarray_Example_get_fun_6@
foreign import ccall unsafe "hs_bindgen_bc6a8e5360af8799" hs_bindgen_bc6a8e5360af8799_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_6@
hs_bindgen_bc6a8e5360af8799 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> IO BG.CInt))
hs_bindgen_bc6a8e5360af8799 =
  BG.fromFFIType hs_bindgen_bc6a8e5360af8799_base

{-# NOINLINE fun_6 #-}
{-| Multi-dimensional array of known size, typedef.

    __C declaration:__ @fun_6@

    __defined at:__ @arrays\/array.h 133:5@

    __exported by:__ @arrays\/array.h@
-}
fun_6 :: BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> IO BG.CInt)
fun_6 =
  BG.unsafePerformIO hs_bindgen_bc6a8e5360af8799

-- __unique:__ @test_arraysarray_Example_get_fun_7@
foreign import ccall unsafe "hs_bindgen_b633a17af8e60afd" hs_bindgen_b633a17af8e60afd_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_7@
hs_bindgen_b633a17af8e60afd :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt))
hs_bindgen_b633a17af8e60afd =
  BG.fromFFIType hs_bindgen_b633a17af8e60afd_base

{-# NOINLINE fun_7 #-}
{-| Multi-dimensional array of unknown size.

    __C declaration:__ @fun_7@

    __defined at:__ @arrays\/array.h 136:5@

    __exported by:__ @arrays\/array.h@
-}
fun_7 :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt)
fun_7 =
  BG.unsafePerformIO hs_bindgen_b633a17af8e60afd

-- __unique:__ @test_arraysarray_Example_get_fun_8@
foreign import ccall unsafe "hs_bindgen_0801bc85e75e7232" hs_bindgen_0801bc85e75e7232_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_8@
hs_bindgen_0801bc85e75e7232 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Tripletlist) -> IO BG.CInt))
hs_bindgen_0801bc85e75e7232 =
  BG.fromFFIType hs_bindgen_0801bc85e75e7232_base

{-# NOINLINE fun_8 #-}
{-| Multi-dimensional array of unknown size, typedef.

    __C declaration:__ @fun_8@

    __defined at:__ @arrays\/array.h 139:5@

    __exported by:__ @arrays\/array.h@
-}
fun_8 :: BG.FunPtr (BG.Ptr (IsA.Elem Tripletlist) -> IO BG.CInt)
fun_8 =
  BG.unsafePerformIO hs_bindgen_0801bc85e75e7232

-- __unique:__ @test_arraysarray_Example_get_isSolved@
foreign import ccall unsafe "hs_bindgen_f525d0058f613e21" hs_bindgen_f525d0058f613e21_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_isSolved@
hs_bindgen_f525d0058f613e21 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Sudoku) -> IO BG.CInt))
hs_bindgen_f525d0058f613e21 =
  BG.fromFFIType hs_bindgen_f525d0058f613e21_base

{-# NOINLINE isSolved #-}
{-| Typedef-in-typedef.

    __C declaration:__ @isSolved@

    __defined at:__ @arrays\/array.h 142:5@

    __exported by:__ @arrays\/array.h@
-}
isSolved :: BG.FunPtr (BG.Ptr (IsA.Elem Sudoku) -> IO BG.CInt)
isSolved =
  BG.unsafePerformIO hs_bindgen_f525d0058f613e21

-- __unique:__ @test_arraysarray_Example_get_fun_1_const@
foreign import ccall unsafe "hs_bindgen_0fdde9c360622daf" hs_bindgen_0fdde9c360622daf_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_1_const@
hs_bindgen_0fdde9c360622daf :: IO (BG.FunPtr (BG.CInt -> BG.Ptr (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> IO BG.CInt))
hs_bindgen_0fdde9c360622daf =
  BG.fromFFIType hs_bindgen_0fdde9c360622daf_base

{-# NOINLINE fun_1_const #-}
{-| Array of known size.

    __C declaration:__ @fun_1_const@

    __defined at:__ @arrays\/array.h 149:5@

    __exported by:__ @arrays\/array.h@
-}
fun_1_const :: BG.FunPtr (BG.CInt -> BG.Ptr (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 3 BG.CInt)) -> IO BG.CInt)
fun_1_const =
  BG.unsafePerformIO hs_bindgen_0fdde9c360622daf

-- __unique:__ @test_arraysarray_Example_get_fun_2_const@
foreign import ccall unsafe "hs_bindgen_8182488430e66fdc" hs_bindgen_8182488430e66fdc_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_2_const@
hs_bindgen_8182488430e66fdc :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Triplet) -> PtrConst.PtrConst (IsA.Elem Triplet) -> IO BG.CInt))
hs_bindgen_8182488430e66fdc =
  BG.fromFFIType hs_bindgen_8182488430e66fdc_base

{-# NOINLINE fun_2_const #-}
{-| Array of known size, typedef.

    __C declaration:__ @fun_2_const@

    __defined at:__ @arrays\/array.h 152:5@

    __exported by:__ @arrays\/array.h@
-}
fun_2_const :: BG.FunPtr (BG.Ptr (IsA.Elem Triplet) -> PtrConst.PtrConst (IsA.Elem Triplet) -> IO BG.CInt)
fun_2_const =
  BG.unsafePerformIO hs_bindgen_8182488430e66fdc

-- __unique:__ @test_arraysarray_Example_get_fun_3_const@
foreign import ccall unsafe "hs_bindgen_2303f5a2f855a6e5" hs_bindgen_2303f5a2f855a6e5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_3_const@
hs_bindgen_2303f5a2f855a6e5 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO BG.CInt))
hs_bindgen_2303f5a2f855a6e5 =
  BG.fromFFIType hs_bindgen_2303f5a2f855a6e5_base

{-# NOINLINE fun_3_const #-}
{-| Array of unknown size.

    __C declaration:__ @fun_3_const@

    __defined at:__ @arrays\/array.h 155:5@

    __exported by:__ @arrays\/array.h@
-}
fun_3_const :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray BG.CInt)) -> PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray BG.CInt)) -> IO BG.CInt)
fun_3_const =
  BG.unsafePerformIO hs_bindgen_2303f5a2f855a6e5

-- __unique:__ @test_arraysarray_Example_get_fun_4_const@
foreign import ccall unsafe "hs_bindgen_4570b26d96819cd9" hs_bindgen_4570b26d96819cd9_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_4_const@
hs_bindgen_4570b26d96819cd9 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem List) -> PtrConst.PtrConst (IsA.Elem List) -> IO BG.CInt))
hs_bindgen_4570b26d96819cd9 =
  BG.fromFFIType hs_bindgen_4570b26d96819cd9_base

{-# NOINLINE fun_4_const #-}
{-| Array of unknown size, typedef.

    __C declaration:__ @fun_4_const@

    __defined at:__ @arrays\/array.h 158:5@

    __exported by:__ @arrays\/array.h@
-}
fun_4_const :: BG.FunPtr (BG.Ptr (IsA.Elem List) -> PtrConst.PtrConst (IsA.Elem List) -> IO BG.CInt)
fun_4_const =
  BG.unsafePerformIO hs_bindgen_4570b26d96819cd9

-- __unique:__ @test_arraysarray_Example_get_fun_5_const@
foreign import ccall unsafe "hs_bindgen_65a5e31cfb885928" hs_bindgen_65a5e31cfb885928_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_5_const@
hs_bindgen_65a5e31cfb885928 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt))
hs_bindgen_65a5e31cfb885928 =
  BG.fromFFIType hs_bindgen_65a5e31cfb885928_base

{-# NOINLINE fun_5_const #-}
{-| Multi-dimensional array of known size.

    __C declaration:__ @fun_5_const@

    __defined at:__ @arrays\/array.h 161:5@

    __exported by:__ @arrays\/array.h@
-}
fun_5_const :: BG.FunPtr (BG.Ptr (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> PtrConst.PtrConst (IsA.Elem (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt)
fun_5_const =
  BG.unsafePerformIO hs_bindgen_65a5e31cfb885928

-- __unique:__ @test_arraysarray_Example_get_fun_6_const@
foreign import ccall unsafe "hs_bindgen_107e8456ef4df268" hs_bindgen_107e8456ef4df268_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_6_const@
hs_bindgen_107e8456ef4df268 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> PtrConst.PtrConst (IsA.Elem Matrix) -> IO BG.CInt))
hs_bindgen_107e8456ef4df268 =
  BG.fromFFIType hs_bindgen_107e8456ef4df268_base

{-# NOINLINE fun_6_const #-}
{-| Multi-dimensional array of known size, typedef.

    __C declaration:__ @fun_6_const@

    __defined at:__ @arrays\/array.h 164:5@

    __exported by:__ @arrays\/array.h@
-}
fun_6_const :: BG.FunPtr (BG.Ptr (IsA.Elem Matrix) -> PtrConst.PtrConst (IsA.Elem Matrix) -> IO BG.CInt)
fun_6_const =
  BG.unsafePerformIO hs_bindgen_107e8456ef4df268

-- __unique:__ @test_arraysarray_Example_get_fun_7_const@
foreign import ccall unsafe "hs_bindgen_249a199499d81ca8" hs_bindgen_249a199499d81ca8_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_7_const@
hs_bindgen_249a199499d81ca8 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt))
hs_bindgen_249a199499d81ca8 =
  BG.fromFFIType hs_bindgen_249a199499d81ca8_base

{-# NOINLINE fun_7_const #-}
{-| Multi-dimensional array of unknown size.

    __C declaration:__ @fun_7_const@

    __defined at:__ @arrays\/array.h 167:5@

    __exported by:__ @arrays\/array.h@
-}
fun_7_const :: BG.FunPtr (BG.Ptr (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> PtrConst.PtrConst (IsA.Elem (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))) -> IO BG.CInt)
fun_7_const =
  BG.unsafePerformIO hs_bindgen_249a199499d81ca8

-- __unique:__ @test_arraysarray_Example_get_fun_8_const@
foreign import ccall unsafe "hs_bindgen_92bc0bfe006ce53c" hs_bindgen_92bc0bfe006ce53c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_8_const@
hs_bindgen_92bc0bfe006ce53c :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Tripletlist) -> PtrConst.PtrConst (IsA.Elem Tripletlist) -> IO BG.CInt))
hs_bindgen_92bc0bfe006ce53c =
  BG.fromFFIType hs_bindgen_92bc0bfe006ce53c_base

{-# NOINLINE fun_8_const #-}
{-| Multi-dimensional array of unknown size, typedef.

    __C declaration:__ @fun_8_const@

    __defined at:__ @arrays\/array.h 170:5@

    __exported by:__ @arrays\/array.h@
-}
fun_8_const :: BG.FunPtr (BG.Ptr (IsA.Elem Tripletlist) -> PtrConst.PtrConst (IsA.Elem Tripletlist) -> IO BG.CInt)
fun_8_const =
  BG.unsafePerformIO hs_bindgen_92bc0bfe006ce53c

-- __unique:__ @test_arraysarray_Example_get_isSolved_const@
foreign import ccall unsafe "hs_bindgen_935428cb3e78f9a5" hs_bindgen_935428cb3e78f9a5_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_isSolved_const@
hs_bindgen_935428cb3e78f9a5 :: IO (BG.FunPtr (BG.Ptr (IsA.Elem Sudoku) -> PtrConst.PtrConst (IsA.Elem Sudoku) -> IO BG.CInt))
hs_bindgen_935428cb3e78f9a5 =
  BG.fromFFIType hs_bindgen_935428cb3e78f9a5_base

{-# NOINLINE isSolved_const #-}
{-| Typedef-in-typedef.

    __C declaration:__ @isSolved_const@

    __defined at:__ @arrays\/array.h 173:5@

    __exported by:__ @arrays\/array.h@
-}
isSolved_const :: BG.FunPtr (BG.Ptr (IsA.Elem Sudoku) -> PtrConst.PtrConst (IsA.Elem Sudoku) -> IO BG.CInt)
isSolved_const =
  BG.unsafePerformIO hs_bindgen_935428cb3e78f9a5

-- __unique:__ @test_arraysarray_Example_get_fun_9@
foreign import ccall unsafe "hs_bindgen_c9bec6ab325b063c" hs_bindgen_c9bec6ab325b063c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_9@
hs_bindgen_c9bec6ab325b063c :: IO (BG.FunPtr (IO (BG.Ptr (CA.ConstantArray 3 BG.CInt))))
hs_bindgen_c9bec6ab325b063c =
  BG.fromFFIType hs_bindgen_c9bec6ab325b063c_base

{-# NOINLINE fun_9 #-}
{-| Array of known size.

    __C declaration:__ @fun_9@

    __defined at:__ @arrays\/array.h 185:7@

    __exported by:__ @arrays\/array.h@
-}
fun_9 :: BG.FunPtr (IO (BG.Ptr (CA.ConstantArray 3 BG.CInt)))
fun_9 =
  BG.unsafePerformIO hs_bindgen_c9bec6ab325b063c

-- __unique:__ @test_arraysarray_Example_get_fun_10@
foreign import ccall unsafe "hs_bindgen_49f85263b6310047" hs_bindgen_49f85263b6310047_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_10@
hs_bindgen_49f85263b6310047 :: IO (BG.FunPtr (IO (BG.Ptr Triplet)))
hs_bindgen_49f85263b6310047 =
  BG.fromFFIType hs_bindgen_49f85263b6310047_base

{-# NOINLINE fun_10 #-}
{-| Array of known size, typedef.

    __C declaration:__ @fun_10@

    __defined at:__ @arrays\/array.h 188:10@

    __exported by:__ @arrays\/array.h@
-}
fun_10 :: BG.FunPtr (IO (BG.Ptr Triplet))
fun_10 =
  BG.unsafePerformIO hs_bindgen_49f85263b6310047

-- __unique:__ @test_arraysarray_Example_get_fun_11@
foreign import ccall unsafe "hs_bindgen_68bdd4a6e02a046b" hs_bindgen_68bdd4a6e02a046b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_11@
hs_bindgen_68bdd4a6e02a046b :: IO (BG.FunPtr (IO (BG.Ptr (IA.IncompleteArray BG.CInt))))
hs_bindgen_68bdd4a6e02a046b =
  BG.fromFFIType hs_bindgen_68bdd4a6e02a046b_base

{-# NOINLINE fun_11 #-}
{-| Array of unknown size.

    __C declaration:__ @fun_11@

    __defined at:__ @arrays\/array.h 191:7@

    __exported by:__ @arrays\/array.h@
-}
fun_11 :: BG.FunPtr (IO (BG.Ptr (IA.IncompleteArray BG.CInt)))
fun_11 =
  BG.unsafePerformIO hs_bindgen_68bdd4a6e02a046b

-- __unique:__ @test_arraysarray_Example_get_fun_12@
foreign import ccall unsafe "hs_bindgen_0521f5208260a928" hs_bindgen_0521f5208260a928_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_12@
hs_bindgen_0521f5208260a928 :: IO (BG.FunPtr (IO (BG.Ptr List)))
hs_bindgen_0521f5208260a928 =
  BG.fromFFIType hs_bindgen_0521f5208260a928_base

{-# NOINLINE fun_12 #-}
{-| Array of unknown size, typedef.

    __C declaration:__ @fun_12@

    __defined at:__ @arrays\/array.h 194:7@

    __exported by:__ @arrays\/array.h@
-}
fun_12 :: BG.FunPtr (IO (BG.Ptr List))
fun_12 =
  BG.unsafePerformIO hs_bindgen_0521f5208260a928

-- __unique:__ @test_arraysarray_Example_get_fun_13@
foreign import ccall unsafe "hs_bindgen_9d0f2421c940811c" hs_bindgen_9d0f2421c940811c_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_13@
hs_bindgen_9d0f2421c940811c :: IO (BG.FunPtr (IO (BG.Ptr (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt)))))
hs_bindgen_9d0f2421c940811c =
  BG.fromFFIType hs_bindgen_9d0f2421c940811c_base

{-# NOINLINE fun_13 #-}
{-| Multi-dimensional array of known size.

    __C declaration:__ @fun_13@

    __defined at:__ @arrays\/array.h 197:7@

    __exported by:__ @arrays\/array.h@
-}
fun_13 :: BG.FunPtr (IO (BG.Ptr (CA.ConstantArray 4 (CA.ConstantArray 3 BG.CInt))))
fun_13 =
  BG.unsafePerformIO hs_bindgen_9d0f2421c940811c

-- __unique:__ @test_arraysarray_Example_get_fun_14@
foreign import ccall unsafe "hs_bindgen_50eeeb460ce92a1d" hs_bindgen_50eeeb460ce92a1d_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_14@
hs_bindgen_50eeeb460ce92a1d :: IO (BG.FunPtr (IO (BG.Ptr Matrix)))
hs_bindgen_50eeeb460ce92a1d =
  BG.fromFFIType hs_bindgen_50eeeb460ce92a1d_base

{-# NOINLINE fun_14 #-}
{-| Multi-dimensional array of known size, typedef.

    __C declaration:__ @fun_14@

    __defined at:__ @arrays\/array.h 200:9@

    __exported by:__ @arrays\/array.h@
-}
fun_14 :: BG.FunPtr (IO (BG.Ptr Matrix))
fun_14 =
  BG.unsafePerformIO hs_bindgen_50eeeb460ce92a1d

-- __unique:__ @test_arraysarray_Example_get_fun_15@
foreign import ccall unsafe "hs_bindgen_afb15d11d3db71eb" hs_bindgen_afb15d11d3db71eb_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_15@
hs_bindgen_afb15d11d3db71eb :: IO (BG.FunPtr (IO (BG.Ptr (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt)))))
hs_bindgen_afb15d11d3db71eb =
  BG.fromFFIType hs_bindgen_afb15d11d3db71eb_base

{-# NOINLINE fun_15 #-}
{-| Multi-dimensional array of unknown size.

    __C declaration:__ @fun_15@

    __defined at:__ @arrays\/array.h 203:7@

    __exported by:__ @arrays\/array.h@
-}
fun_15 :: BG.FunPtr (IO (BG.Ptr (IA.IncompleteArray (CA.ConstantArray 3 BG.CInt))))
fun_15 =
  BG.unsafePerformIO hs_bindgen_afb15d11d3db71eb

-- __unique:__ @test_arraysarray_Example_get_fun_16@
foreign import ccall unsafe "hs_bindgen_f41fbdca0717f85b" hs_bindgen_f41fbdca0717f85b_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_fun_16@
hs_bindgen_f41fbdca0717f85b :: IO (BG.FunPtr (IO (BG.Ptr Tripletlist)))
hs_bindgen_f41fbdca0717f85b =
  BG.fromFFIType hs_bindgen_f41fbdca0717f85b_base

{-# NOINLINE fun_16 #-}
{-| Multi-dimensional array of unknown size, typedef.

    __C declaration:__ @fun_16@

    __defined at:__ @arrays\/array.h 206:14@

    __exported by:__ @arrays\/array.h@
-}
fun_16 :: BG.FunPtr (IO (BG.Ptr Tripletlist))
fun_16 =
  BG.unsafePerformIO hs_bindgen_f41fbdca0717f85b

-- __unique:__ @test_arraysarray_Example_get_solve@
foreign import ccall unsafe "hs_bindgen_b219669884cf1eb6" hs_bindgen_b219669884cf1eb6_base ::
     IO (BG.FunPtr BG.Void)

-- __unique:__ @test_arraysarray_Example_get_solve@
hs_bindgen_b219669884cf1eb6 :: IO (BG.FunPtr (IO (BG.Ptr Sudoku)))
hs_bindgen_b219669884cf1eb6 =
  BG.fromFFIType hs_bindgen_b219669884cf1eb6_base

{-# NOINLINE solve #-}
{-| Typedef-in-typedef.

    __C declaration:__ @solve@

    __defined at:__ @arrays\/array.h 209:10@

    __exported by:__ @arrays\/array.h@
-}
solve :: BG.FunPtr (IO (BG.Ptr Sudoku))
solve =
  BG.unsafePerformIO hs_bindgen_b219669884cf1eb6
