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
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "/* test_arraysarray_Example_get_fun_1_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3da43df5677c71ad (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3]"
  , ")"
  , "{"
  , "  return &fun_1;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_2170297251bf6d62 (void)) ("
  , "  triplet arg1"
  , ")"
  , "{"
  , "  return &fun_2;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a90e84da83866d0e (void)) ("
  , "  signed int arg1[]"
  , ")"
  , "{"
  , "  return &fun_3;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_93e48e07f9f40577 (void)) ("
  , "  list arg1"
  , ")"
  , "{"
  , "  return &fun_4;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3fadc044f8437855 (void)) ("
  , "  signed int arg1[4][3]"
  , ")"
  , "{"
  , "  return &fun_5;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4b116cc6e48e6c3b (void)) ("
  , "  matrix arg1"
  , ")"
  , "{"
  , "  return &fun_6;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_27f76815dbc61f73 (void)) ("
  , "  signed int arg1[][3]"
  , ")"
  , "{"
  , "  return &fun_7;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_a79b67b394d1dab8 (void)) ("
  , "  tripletlist arg1"
  , ")"
  , "{"
  , "  return &fun_8;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_3035f04158da4ea8 (void)) ("
  , "  sudoku arg1"
  , ")"
  , "{"
  , "  return &isSolved;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_1_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_4ca938a03ef0961a (void)) ("
  , "  signed int arg1,"
  , "  signed int arg2[3],"
  , "  signed int const arg3[3]"
  , ")"
  , "{"
  , "  return &fun_1_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_2_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_ab436eab87e0d868 (void)) ("
  , "  triplet arg1,"
  , "  triplet const arg2"
  , ")"
  , "{"
  , "  return &fun_2_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_3_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_98d06bd5403ada68 (void)) ("
  , "  signed int arg1[],"
  , "  signed int const arg2[]"
  , ")"
  , "{"
  , "  return &fun_3_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_4_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_73a3249ecd4b2587 (void)) ("
  , "  list arg1,"
  , "  list const arg2"
  , ")"
  , "{"
  , "  return &fun_4_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_5_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7a4270e16880a707 (void)) ("
  , "  signed int arg1[4][3],"
  , "  signed int const arg2[4][3]"
  , ")"
  , "{"
  , "  return &fun_5_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_6_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_7d046eec920d0789 (void)) ("
  , "  matrix arg1,"
  , "  matrix const arg2"
  , ")"
  , "{"
  , "  return &fun_6_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_7_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_e60c9fdf601f4d52 (void)) ("
  , "  signed int arg1[][3],"
  , "  signed int const arg2[][3]"
  , ")"
  , "{"
  , "  return &fun_7_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_8_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_26377cb588f993f2 (void)) ("
  , "  tripletlist arg1,"
  , "  tripletlist const arg2"
  , ")"
  , "{"
  , "  return &fun_8_const;"
  , "}"
  , "/* test_arraysarray_Example_get_isSolved_const_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*hs_bindgen_525c462baff9c281 (void)) ("
  , "  sudoku arg1,"
  , "  sudoku const arg2"
  , ")"
  , "{"
  , "  return &isSolved_const;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_9_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_1ee64a8054febdc1 (void)) (void))[3]"
  , "{"
  , "  return &fun_9;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_10_ptr */"
  , "__attribute__ ((const))"
  , "triplet *(*hs_bindgen_c8090d6b86a88ba0 (void)) (void)"
  , "{"
  , "  return &fun_10;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_11_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_4f90fd6464df2b20 (void)) (void))[]"
  , "{"
  , "  return &fun_11;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_12_ptr */"
  , "__attribute__ ((const))"
  , "list *(*hs_bindgen_4b4a73f20be545eb (void)) (void)"
  , "{"
  , "  return &fun_12;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_13_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_a88be261251caf90 (void)) (void))[4][3]"
  , "{"
  , "  return &fun_13;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_14_ptr */"
  , "__attribute__ ((const))"
  , "matrix *(*hs_bindgen_2f0a2188338306d9 (void)) (void)"
  , "{"
  , "  return &fun_14;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_15_ptr */"
  , "__attribute__ ((const))"
  , "signed int (*(*hs_bindgen_30af82288a309775 (void)) (void))[][3]"
  , "{"
  , "  return &fun_15;"
  , "}"
  , "/* test_arraysarray_Example_get_fun_16_ptr */"
  , "__attribute__ ((const))"
  , "tripletlist *(*hs_bindgen_fb63d18d5d1004fb (void)) (void)"
  , "{"
  , "  return &fun_16;"
  , "}"
  , "/* test_arraysarray_Example_get_solve_ptr */"
  , "__attribute__ ((const))"
  , "sudoku *(*hs_bindgen_e7d751562a2e3c6c (void)) (void)"
  , "{"
  , "  return &solve;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3da43df5677c71ad" hs_bindgen_3da43df5677c71ad_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_1_ptr@
hs_bindgen_3da43df5677c71ad ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))
hs_bindgen_3da43df5677c71ad =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3da43df5677c71ad_base

{-# NOINLINE fun_1_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3da43df5677c71ad

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2170297251bf6d62" hs_bindgen_2170297251bf6d62_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_2_ptr@
hs_bindgen_2170297251bf6d62 ::
     IO (Ptr.FunPtr (Triplet -> IO FC.CInt))
hs_bindgen_2170297251bf6d62 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2170297251bf6d62_base

{-# NOINLINE fun_2_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_ptr :: Ptr.FunPtr (Triplet -> IO FC.CInt)
fun_2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2170297251bf6d62

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a90e84da83866d0e" hs_bindgen_a90e84da83866d0e_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_3_ptr@
hs_bindgen_a90e84da83866d0e ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))
hs_bindgen_a90e84da83866d0e =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a90e84da83866d0e_base

{-# NOINLINE fun_3_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a90e84da83866d0e

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_93e48e07f9f40577" hs_bindgen_93e48e07f9f40577_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_4_ptr@
hs_bindgen_93e48e07f9f40577 ::
     IO (Ptr.FunPtr (List -> IO FC.CInt))
hs_bindgen_93e48e07f9f40577 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_93e48e07f9f40577_base

{-# NOINLINE fun_4_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_ptr :: Ptr.FunPtr (List -> IO FC.CInt)
fun_4_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_93e48e07f9f40577

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3fadc044f8437855" hs_bindgen_3fadc044f8437855_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_5_ptr@
hs_bindgen_3fadc044f8437855 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))
hs_bindgen_3fadc044f8437855 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3fadc044f8437855_base

{-# NOINLINE fun_5_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3fadc044f8437855

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4b116cc6e48e6c3b" hs_bindgen_4b116cc6e48e6c3b_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_6_ptr@
hs_bindgen_4b116cc6e48e6c3b ::
     IO (Ptr.FunPtr (Matrix -> IO FC.CInt))
hs_bindgen_4b116cc6e48e6c3b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4b116cc6e48e6c3b_base

{-# NOINLINE fun_6_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_ptr :: Ptr.FunPtr (Matrix -> IO FC.CInt)
fun_6_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4b116cc6e48e6c3b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_27f76815dbc61f73" hs_bindgen_27f76815dbc61f73_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_7_ptr@
hs_bindgen_27f76815dbc61f73 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))
hs_bindgen_27f76815dbc61f73 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_27f76815dbc61f73_base

{-# NOINLINE fun_7_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_27f76815dbc61f73

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a79b67b394d1dab8" hs_bindgen_a79b67b394d1dab8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_8_ptr@
hs_bindgen_a79b67b394d1dab8 ::
     IO (Ptr.FunPtr (Tripletlist -> IO FC.CInt))
hs_bindgen_a79b67b394d1dab8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a79b67b394d1dab8_base

{-# NOINLINE fun_8_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_ptr :: Ptr.FunPtr (Tripletlist -> IO FC.CInt)
fun_8_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a79b67b394d1dab8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3035f04158da4ea8" hs_bindgen_3035f04158da4ea8_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_isSolved_ptr@
hs_bindgen_3035f04158da4ea8 ::
     IO (Ptr.FunPtr (Sudoku -> IO FC.CInt))
hs_bindgen_3035f04158da4ea8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3035f04158da4ea8_base

{-# NOINLINE isSolved_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_ptr :: Ptr.FunPtr (Sudoku -> IO FC.CInt)
isSolved_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3035f04158da4ea8

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4ca938a03ef0961a" hs_bindgen_4ca938a03ef0961a_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_1_const_ptr@
hs_bindgen_4ca938a03ef0961a ::
     IO (Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt))
hs_bindgen_4ca938a03ef0961a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4ca938a03ef0961a_base

{-# NOINLINE fun_1_const_ptr #-}

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const_ptr :: Ptr.FunPtr (FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt)
fun_1_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4ca938a03ef0961a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ab436eab87e0d868" hs_bindgen_ab436eab87e0d868_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_2_const_ptr@
hs_bindgen_ab436eab87e0d868 ::
     IO (Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt))
hs_bindgen_ab436eab87e0d868 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_ab436eab87e0d868_base

{-# NOINLINE fun_2_const_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const_ptr :: Ptr.FunPtr (Triplet -> Triplet -> IO FC.CInt)
fun_2_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ab436eab87e0d868

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_98d06bd5403ada68" hs_bindgen_98d06bd5403ada68_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_3_const_ptr@
hs_bindgen_98d06bd5403ada68 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt))
hs_bindgen_98d06bd5403ada68 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_98d06bd5403ada68_base

{-# NOINLINE fun_3_const_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt)
fun_3_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_98d06bd5403ada68

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_73a3249ecd4b2587" hs_bindgen_73a3249ecd4b2587_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_4_const_ptr@
hs_bindgen_73a3249ecd4b2587 ::
     IO (Ptr.FunPtr (List -> List -> IO FC.CInt))
hs_bindgen_73a3249ecd4b2587 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_73a3249ecd4b2587_base

{-# NOINLINE fun_4_const_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const_ptr :: Ptr.FunPtr (List -> List -> IO FC.CInt)
fun_4_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_73a3249ecd4b2587

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7a4270e16880a707" hs_bindgen_7a4270e16880a707_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_5_const_ptr@
hs_bindgen_7a4270e16880a707 ::
     IO (Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))
hs_bindgen_7a4270e16880a707 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7a4270e16880a707_base

{-# NOINLINE fun_5_const_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const_ptr :: Ptr.FunPtr (((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_5_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7a4270e16880a707

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7d046eec920d0789" hs_bindgen_7d046eec920d0789_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_6_const_ptr@
hs_bindgen_7d046eec920d0789 ::
     IO (Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt))
hs_bindgen_7d046eec920d0789 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7d046eec920d0789_base

{-# NOINLINE fun_6_const_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const_ptr :: Ptr.FunPtr (Matrix -> Matrix -> IO FC.CInt)
fun_6_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7d046eec920d0789

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e60c9fdf601f4d52" hs_bindgen_e60c9fdf601f4d52_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_7_const_ptr@
hs_bindgen_e60c9fdf601f4d52 ::
     IO (Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt))
hs_bindgen_e60c9fdf601f4d52 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e60c9fdf601f4d52_base

{-# NOINLINE fun_7_const_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const_ptr :: Ptr.FunPtr ((HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt)
fun_7_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e60c9fdf601f4d52

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_26377cb588f993f2" hs_bindgen_26377cb588f993f2_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_8_const_ptr@
hs_bindgen_26377cb588f993f2 ::
     IO (Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt))
hs_bindgen_26377cb588f993f2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_26377cb588f993f2_base

{-# NOINLINE fun_8_const_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const_ptr :: Ptr.FunPtr (Tripletlist -> Tripletlist -> IO FC.CInt)
fun_8_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_26377cb588f993f2

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_525c462baff9c281" hs_bindgen_525c462baff9c281_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_isSolved_const_ptr@
hs_bindgen_525c462baff9c281 ::
     IO (Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt))
hs_bindgen_525c462baff9c281 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_525c462baff9c281_base

{-# NOINLINE isSolved_const_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const_ptr :: Ptr.FunPtr (Sudoku -> Sudoku -> IO FC.CInt)
isSolved_const_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_525c462baff9c281

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1ee64a8054febdc1" hs_bindgen_1ee64a8054febdc1_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_9_ptr@
hs_bindgen_1ee64a8054febdc1 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
hs_bindgen_1ee64a8054febdc1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1ee64a8054febdc1_base

{-# NOINLINE fun_9_ptr #-}

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_9_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1ee64a8054febdc1

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c8090d6b86a88ba0" hs_bindgen_c8090d6b86a88ba0_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_10_ptr@
hs_bindgen_c8090d6b86a88ba0 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Triplet)))
hs_bindgen_c8090d6b86a88ba0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c8090d6b86a88ba0_base

{-# NOINLINE fun_10_ptr #-}

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Triplet))
fun_10_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c8090d6b86a88ba0

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4f90fd6464df2b20" hs_bindgen_4f90fd6464df2b20_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_11_ptr@
hs_bindgen_4f90fd6464df2b20 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))))
hs_bindgen_4f90fd6464df2b20 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4f90fd6464df2b20_base

{-# NOINLINE fun_11_ptr #-}

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)))
fun_11_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f90fd6464df2b20

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4b4a73f20be545eb" hs_bindgen_4b4a73f20be545eb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_12_ptr@
hs_bindgen_4b4a73f20be545eb ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr List)))
hs_bindgen_4b4a73f20be545eb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4b4a73f20be545eb_base

{-# NOINLINE fun_12_ptr #-}

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12_ptr :: Ptr.FunPtr (IO (Ptr.Ptr List))
fun_12_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4b4a73f20be545eb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a88be261251caf90" hs_bindgen_a88be261251caf90_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_13_ptr@
hs_bindgen_a88be261251caf90 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_a88be261251caf90 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a88be261251caf90_base

{-# NOINLINE fun_13_ptr #-}

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13_ptr :: Ptr.FunPtr (IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_13_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a88be261251caf90

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2f0a2188338306d9" hs_bindgen_2f0a2188338306d9_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_14_ptr@
hs_bindgen_2f0a2188338306d9 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Matrix)))
hs_bindgen_2f0a2188338306d9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2f0a2188338306d9_base

{-# NOINLINE fun_14_ptr #-}

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Matrix))
fun_14_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2f0a2188338306d9

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_30af82288a309775" hs_bindgen_30af82288a309775_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_15_ptr@
hs_bindgen_30af82288a309775 ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))))
hs_bindgen_30af82288a309775 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_30af82288a309775_base

{-# NOINLINE fun_15_ptr #-}

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15_ptr :: Ptr.FunPtr (IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))))
fun_15_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_30af82288a309775

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_fb63d18d5d1004fb" hs_bindgen_fb63d18d5d1004fb_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_fun_16_ptr@
hs_bindgen_fb63d18d5d1004fb ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Tripletlist)))
hs_bindgen_fb63d18d5d1004fb =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_fb63d18d5d1004fb_base

{-# NOINLINE fun_16_ptr #-}

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Tripletlist))
fun_16_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fb63d18d5d1004fb

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e7d751562a2e3c6c" hs_bindgen_e7d751562a2e3c6c_base ::
     IO (Ptr.FunPtr Void)

-- | __unique:__ @test_arraysarray_Example_get_solve_ptr@
hs_bindgen_e7d751562a2e3c6c ::
     IO (Ptr.FunPtr (IO (Ptr.Ptr Sudoku)))
hs_bindgen_e7d751562a2e3c6c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e7d751562a2e3c6c_base

{-# NOINLINE solve_ptr #-}

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve_ptr :: Ptr.FunPtr (IO (Ptr.Ptr Sudoku))
solve_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e7d751562a2e3c6c
