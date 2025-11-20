{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Marshallable
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "signed int hs_bindgen_test_arraysarray_5d1be223fd040c3b ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_cabe35537b18e986 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_4cdbf10236e78984 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_e356c5ddb2608063 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_4(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_f5ccf2c8d2e60be5 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_5(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_2b3a983697999524 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_6(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_72e9371a1b8b8907 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_7(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_62ad87463d9a75de ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_8(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_2280ecc4c152a73f ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return isSolved(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_f1d120f83dc61db5 ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const *arg3"
  , ")"
  , "{"
  , "  return fun_1_const(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_f15760e6f3596189 ("
  , "  signed int *arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_2_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_0ad99f041fc4f5ca ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return fun_3_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_d61f2b8777e6ca19 ("
  , "  signed int *arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_4_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_9e1f66e6a0369c45 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_5_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_5b4bd3c6cee83e61 ("
  , "  signed int (*arg1)[3],"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_6_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_b551069ce9e1f12e ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_7_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_4ac495707a95aa13 ("
  , "  signed int (*arg1)[3],"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_8_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_1bdcfcd7aca9a2f6 ("
  , "  triplet *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  return isSolved_const(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_d4c729a69c884fd4 (void))[3]"
  , "{"
  , "  return fun_9();"
  , "}"
  , "triplet *hs_bindgen_test_arraysarray_bb92dfded907271e (void)"
  , "{"
  , "  return fun_10();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_489aaaa59e992ddf (void))[]"
  , "{"
  , "  return fun_11();"
  , "}"
  , "list *hs_bindgen_test_arraysarray_ee94c35f987d6c50 (void)"
  , "{"
  , "  return fun_12();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_ca2c7b60ce85a964 (void))[4][3]"
  , "{"
  , "  return fun_13();"
  , "}"
  , "matrix *hs_bindgen_test_arraysarray_ab2c533efdae8e41 (void)"
  , "{"
  , "  return fun_14();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_019bdeb5db79cee1 (void))[][3]"
  , "{"
  , "  return fun_15();"
  , "}"
  , "tripletlist *hs_bindgen_test_arraysarray_ca0e7c51654fef12 (void)"
  , "{"
  , "  return fun_16();"
  , "}"
  , "sudoku *hs_bindgen_test_arraysarray_f6b66497ee1685b0 (void)"
  , "{"
  , "  return solve();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_5d1be223fd040c3b" fun_1_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1 ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt
fun_1 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_1_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_cabe35537b18e986" fun_2_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt
fun_2 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_2_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_4cdbf10236e78984" fun_3_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt
fun_3 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_3_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_e356c5ddb2608063" fun_4_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt
fun_4 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_4_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_f5ccf2c8d2e60be5" fun_5_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt
fun_5 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_5_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_2b3a983697999524" fun_6_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt
fun_6 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_6_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_72e9371a1b8b8907" fun_7_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt
fun_7 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_7_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_62ad87463d9a75de" fun_8_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt
fun_8 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_8_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_2280ecc4c152a73f" isSolved_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr Triplet
    -> IO FC.CInt
    )

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved ::
     Ptr.Ptr Triplet
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt
isSolved =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType isSolved_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_f1d120f83dc61db5" fun_1_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       FC.CInt
    -> Ptr.Ptr FC.CInt
    -> Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_1_const'
-}
fun_1_const_wrapper ::
     FC.CInt
  -> Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt
fun_1_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_1_const_wrapper_base

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt
fun_1_const =
  \x0 ->
    \x1 ->
      \x2 ->
        HsBindgen.Runtime.ConstantArray.withPtr x2 (\ptr3 ->
                                                      fun_1_const_wrapper x0 x1 ptr3)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_f15760e6f3596189" fun_2_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_2_const'
-}
fun_2_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt
fun_2_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_2_const_wrapper_base

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> Triplet
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt
fun_2_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_2_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_0ad99f041fc4f5ca" fun_3_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_3_const'
-}
fun_3_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt
fun_3_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_3_const_wrapper_base

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt
fun_3_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_3_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_d61f2b8777e6ca19" fun_4_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr FC.CInt
    -> Ptr.Ptr FC.CInt
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_4_const'
-}
fun_4_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt
fun_4_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_4_const_wrapper_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> List
     {- ^ __C declaration:__ @ys@
     -}
  -> IO FC.CInt
fun_4_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_4_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_9e1f66e6a0369c45" fun_5_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_5_const'
-}
fun_5_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_5_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_5_const_wrapper_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt
fun_5_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_5_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_5b4bd3c6cee83e61" fun_6_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_6_const'
-}
fun_6_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_6_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_6_const_wrapper_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Matrix
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt
fun_6_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_6_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_b551069ce9e1f12e" fun_7_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_7_const'
-}
fun_7_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_7_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_7_const_wrapper_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt
fun_7_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_7_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_4ac495707a95aa13" fun_8_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
    -> IO FC.CInt
    )

{-| Pointer-based API for 'fun_8_const'
-}
fun_8_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_8_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_8_const_wrapper_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> Tripletlist
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt
fun_8_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_8_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_1bdcfcd7aca9a2f6" isSolved_const_wrapper_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       Ptr.Ptr Triplet
    -> Ptr.Ptr Triplet
    -> IO FC.CInt
    )

{-| Pointer-based API for 'isSolved_const'
-}
isSolved_const_wrapper ::
     Ptr.Ptr Triplet
  -> Ptr.Ptr Triplet
  -> IO FC.CInt
isSolved_const_wrapper =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType isSolved_const_wrapper_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const ::
     Ptr.Ptr Triplet
     {- ^ __C declaration:__ @xss@
     -}
  -> Sudoku
     {- ^ __C declaration:__ @yss@
     -}
  -> IO FC.CInt
isSolved_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    isSolved_const_wrapper x0 ptr2)

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_d4c729a69c884fd4" fun_9_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
    )

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
fun_9 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_9_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_bb92dfded907271e" fun_10_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr Triplet)
    )

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 ::
     IO (Ptr.Ptr Triplet)
fun_10 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_10_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_489aaaa59e992ddf" fun_11_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
    )

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
fun_11 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_11_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_ee94c35f987d6c50" fun_12_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr List)
    )

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 ::
     IO (Ptr.Ptr List)
fun_12 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_12_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_ca2c7b60ce85a964" fun_13_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
    )

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_13 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_13_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_ab2c533efdae8e41" fun_14_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr Matrix)
    )

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 ::
     IO (Ptr.Ptr Matrix)
fun_14 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_14_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_019bdeb5db79cee1" fun_15_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
    )

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_15 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_15_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_ca0e7c51654fef12" fun_16_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr Tripletlist)
    )

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 ::
     IO (Ptr.Ptr Tripletlist)
fun_16 =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType fun_16_base

{-| This is an internal function.
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_f6b66497ee1685b0" solve_base ::
  HsBindgen.Runtime.Marshallable.MarshallableBaseType (
       IO (Ptr.Ptr Sudoku)
    )

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve ::
     IO (Ptr.Ptr Sudoku)
solve =
  HsBindgen.Runtime.Marshallable.fromMarshallableBaseType solve_base
