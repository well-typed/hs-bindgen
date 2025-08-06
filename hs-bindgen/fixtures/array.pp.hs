{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include <array.h>\n__attribute__ ((const)) signed int (*get_arr0_ptr (void))[3] { return &arr0; } \n__attribute__ ((const)) signed int (*get_arr1_ptr (void))[3] { return &arr1; } \n__attribute__ ((const)) signed int (*get_arr2_ptr (void))[3] { return &arr2; } \n__attribute__ ((const)) signed int (*get_arr3_ptr (void))[3] { return &arr3; } \n__attribute__ ((const)) signed int (*get_arr6_ptr (void))[1] { return &arr6; } \n__attribute__ ((const)) signed int (*get_arr7_ptr (void))[] { return &arr7; } \n__attribute__ ((const)) signed int (*get_arr_1_ptr (void))[3] { return &arr_1; } \n__attribute__ ((const)) triplet *get_arr_2_ptr (void) { return &arr_2; } \n__attribute__ ((const)) signed int (*get_arr_3_ptr (void))[] { return &arr_3; } \n__attribute__ ((const)) list *get_arr_4_ptr (void) { return &arr_4; } \n__attribute__ ((const)) signed int (*get_arr_5_ptr (void))[4][3] { return &arr_5; } \n__attribute__ ((const)) matrix *get_arr_6_ptr (void) { return &arr_6; } \n__attribute__ ((const)) signed int (*get_arr_7_ptr (void))[][3] { return &arr_7; } \n__attribute__ ((const)) tripletlist *get_arr_8_ptr (void) { return &arr_8; } \nsigned int testmodule_fun_1 (signed int arg1, signed int *arg2) { return fun_1(arg1, arg2); }\nsigned int testmodule_fun_2 (signed int *arg1) { return fun_2(arg1); }\nsigned int testmodule_fun_3 (signed int *arg1) { return fun_3(arg1); }\nsigned int testmodule_fun_4 (signed int *arg1) { return fun_4(arg1); }\nsigned int testmodule_fun_5 (signed int (*arg1)[3]) { return fun_5(arg1); }\nsigned int testmodule_fun_6 (signed int (*arg1)[3]) { return fun_6(arg1); }\nsigned int testmodule_fun_7 (signed int (*arg1)[3]) { return fun_7(arg1); }\nsigned int testmodule_fun_8 (signed int (*arg1)[3]) { return fun_8(arg1); }\nsigned int (*testmodule_fun_9 (void))[3] { return fun_9(); }\ntriplet *testmodule_fun_10 (void) { return fun_10(); }\nsigned int (*testmodule_fun_11 (void))[] { return fun_11(); }\nlist *testmodule_fun_12 (void) { return fun_12(); }\nsigned int (*testmodule_fun_13 (void))[4][3] { return fun_13(); }\nmatrix *testmodule_fun_14 (void) { return fun_14(); }\nsigned int (*testmodule_fun_15 (void))[][3] { return fun_15(); }\ntripletlist *testmodule_fun_16 (void) { return fun_16(); }\n")

{-| Global, complete, not initialised

  __from C:__ @arr0@
-}
foreign import ccall safe "get_arr0_ptr" arr0 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

{-| Global, complete, initialised

  __from C:__ @arr1@
-}
foreign import ccall safe "get_arr1_ptr" arr1 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

{-| Global, extern, complete, not initialised

  __from C:__ @arr2@
-}
foreign import ccall safe "get_arr2_ptr" arr2 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

{-| Global, extern, complete, initialised

  __from C:__ @arr3@
-}
foreign import ccall safe "get_arr3_ptr" arr3 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

{-| Global, incomplete

  __from C:__ @arr6@
-}
foreign import ccall safe "get_arr6_ptr" arr6 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 1) FC.CInt)

{-| Global, extern, incomplete

  __from C:__ @arr7@
-}
foreign import ccall safe "get_arr7_ptr" arr7 :: F.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)

newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

newtype List = List
  { un_List :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

newtype Tripletlist = Tripletlist
  { un_Tripletlist :: HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  }
  deriving stock (Eq, Show)

data Example = Example
  { example_triple :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  , example_sudoku :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
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

{-| Array of known size

  __from C:__ @arr_1@
-}
foreign import ccall safe "get_arr_1_ptr" arr_1 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)

{-| Array of known size, typedef

  __from C:__ @arr_2@
-}
foreign import ccall safe "get_arr_2_ptr" arr_2 :: F.Ptr Triplet

{-| Array of unknown size

  __from C:__ @arr_3@
-}
foreign import ccall safe "get_arr_3_ptr" arr_3 :: F.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)

{-| Array of unknown size, typedef

  __from C:__ @arr_4@
-}
foreign import ccall safe "get_arr_4_ptr" arr_4 :: F.Ptr List

{-| Multi-dimensional array of known size

  __from C:__ @arr_5@
-}
foreign import ccall safe "get_arr_5_ptr" arr_5 :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Multi-dimensional array of known size, typedef

  __from C:__ @arr_6@
-}
foreign import ccall safe "get_arr_6_ptr" arr_6 :: F.Ptr Matrix

{-| Multi-dimensional array of unknown size

  __from C:__ @arr_7@
-}
foreign import ccall safe "get_arr_7_ptr" arr_7 :: F.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Multi-dimensional array of unknown size, typedef

  __from C:__ @arr_8@
-}
foreign import ccall safe "get_arr_8_ptr" arr_8 :: F.Ptr Tripletlist

{-| Array of known size

  __from C:__ @fun_1(int, int *)@
-}
foreign import ccall safe "testmodule_fun_1" fun_1_wrapper :: FC.CInt -> (F.Ptr FC.CInt) -> IO FC.CInt

fun_1 :: FC.CInt -> ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt) -> IO FC.CInt
fun_1 =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_1_wrapper x0 ptr2)

{-| Array of known size, typedef

  __from C:__ @fun_2(int *)@
-}
foreign import ccall safe "testmodule_fun_2" fun_2_wrapper :: (F.Ptr FC.CInt) -> IO FC.CInt

fun_2 :: Triplet -> IO FC.CInt
fun_2 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_2_wrapper ptr1)

{-| Array of unknown size

  __from C:__ @fun_3(int *)@
-}
foreign import ccall safe "testmodule_fun_3" fun_3_wrapper :: (F.Ptr FC.CInt) -> IO FC.CInt

fun_3 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt) -> IO FC.CInt
fun_3 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_3_wrapper ptr1)

{-| Array of unknown size, typedef

  __from C:__ @fun_4(int *)@
-}
foreign import ccall safe "testmodule_fun_4" fun_4_wrapper :: (F.Ptr FC.CInt) -> IO FC.CInt

fun_4 :: List -> IO FC.CInt
fun_4 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_4_wrapper ptr1)

{-| Multi-dimensional array of known size

  __from C:__ @fun_5(int (*)[3])@
-}
foreign import ccall safe "testmodule_fun_5" fun_5_wrapper :: (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt

fun_5 :: ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_5 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_5_wrapper ptr1)

{-| Multi-dimensional array of known size, typedef

  __from C:__ @fun_6(int (*)[3])@
-}
foreign import ccall safe "testmodule_fun_6" fun_6_wrapper :: (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt

fun_6 :: Matrix -> IO FC.CInt
fun_6 =
  \x0 ->
    HsBindgen.Runtime.ConstantArray.withPtr x0 (\ptr1 ->
                                                  fun_6_wrapper ptr1)

{-| Multi-dimensional array of unknown size

  __from C:__ @fun_7(int (*)[3])@
-}
foreign import ccall safe "testmodule_fun_7" fun_7_wrapper :: (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt

fun_7 :: (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt
fun_7 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_7_wrapper ptr1)

{-| Multi-dimensional array of unknown size, typedef

  __from C:__ @fun_8(int (*)[3])@
-}
foreign import ccall safe "testmodule_fun_8" fun_8_wrapper :: (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)) -> IO FC.CInt

fun_8 :: Tripletlist -> IO FC.CInt
fun_8 =
  \x0 ->
    HsBindgen.Runtime.IncompleteArray.withPtr x0 (\ptr1 ->
                                                    fun_8_wrapper ptr1)

{-| Array of known size

  __from C:__ @fun_9()@
-}
foreign import ccall safe "testmodule_fun_9" fun_9 :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Array of known size, typedef

  __from C:__ @fun_10()@
-}
foreign import ccall safe "testmodule_fun_10" fun_10 :: IO (F.Ptr Triplet)

{-| Array of unknown size

  __from C:__ @fun_11()@
-}
foreign import ccall safe "testmodule_fun_11" fun_11 :: IO (F.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-| Array of unknown size, typedef

  __from C:__ @fun_12()@
-}
foreign import ccall safe "testmodule_fun_12" fun_12 :: IO (F.Ptr List)

{-| Multi-dimensional array of known size

  __from C:__ @fun_13()@
-}
foreign import ccall safe "testmodule_fun_13" fun_13 :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of known size, typedef

  __from C:__ @fun_14()@
-}
foreign import ccall safe "testmodule_fun_14" fun_14 :: IO (F.Ptr Matrix)

{-| Multi-dimensional array of unknown size

  __from C:__ @fun_15()@
-}
foreign import ccall safe "testmodule_fun_15" fun_15 :: IO (F.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of unknown size, typedef

  __from C:__ @fun_16()@
-}
foreign import ccall safe "testmodule_fun_16" fun_16 :: IO (F.Ptr Tripletlist)
