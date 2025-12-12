{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "signed int hs_bindgen_6d07a0b03f884547 ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_04318f98a3ab8d08 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_2(arg1);"
  , "}"
  , "signed int hs_bindgen_2a7c5fa1040fa8db ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_3(arg1);"
  , "}"
  , "signed int hs_bindgen_810acc5cf8729d0e ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_4(arg1);"
  , "}"
  , "signed int hs_bindgen_83b71f7defb3b27a ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_5(arg1);"
  , "}"
  , "signed int hs_bindgen_62b76af3dc65da3f ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_6(arg1);"
  , "}"
  , "signed int hs_bindgen_100aa7fb87a5ea74 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_7(arg1);"
  , "}"
  , "signed int hs_bindgen_cd6646babeacd609 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_8(arg1);"
  , "}"
  , "signed int hs_bindgen_560f1de9a83c3a6a ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return isSolved(arg1);"
  , "}"
  , "signed int hs_bindgen_ef3b85ae74bc06cf ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const *arg3"
  , ")"
  , "{"
  , "  return fun_1_const(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_1c913685e5e76952 ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return fun_2_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_eb8daf22bd5c6f00 ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return fun_3_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_0b73e4c7695a3b2f ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return fun_4_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_374feb8086895fe3 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_5_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_2d1320b468c36708 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const *arg2[3]"
  , ")"
  , "{"
  , "  return fun_6_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_f67f5fe5bfb57aa1 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_7_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_2c9356851d76320e ("
  , "  signed int (*arg1)[3],"
  , "  signed int const *arg2[3]"
  , ")"
  , "{"
  , "  return fun_8_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_39b08b64fed0c5b8 ("
  , "  triplet *arg1,"
  , "  triplet const *arg2"
  , ")"
  , "{"
  , "  return isSolved_const(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_ab431ebc0519545a (void))[3]"
  , "{"
  , "  return fun_9();"
  , "}"
  , "triplet *hs_bindgen_c9ff623e6f48d3bc (void)"
  , "{"
  , "  return fun_10();"
  , "}"
  , "signed int (*hs_bindgen_e714f0b7c764ba17 (void))[]"
  , "{"
  , "  return fun_11();"
  , "}"
  , "list *hs_bindgen_cc23741700ba18f7 (void)"
  , "{"
  , "  return fun_12();"
  , "}"
  , "signed int (*hs_bindgen_eb3a1364003829ac (void))[4][3]"
  , "{"
  , "  return fun_13();"
  , "}"
  , "matrix *hs_bindgen_0f49ffbe2c13ab46 (void)"
  , "{"
  , "  return fun_14();"
  , "}"
  , "signed int (*hs_bindgen_59de769fbba4ed72 (void))[][3]"
  , "{"
  , "  return fun_15();"
  , "}"
  , "tripletlist *hs_bindgen_1d6ecccfa4ee16ff (void)"
  , "{"
  , "  return fun_16();"
  , "}"
  , "sudoku *hs_bindgen_6165085eab7d2806 (void)"
  , "{"
  , "  return solve();"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6d07a0b03f884547" fun_1_base ::
     FC.CInt
  -> Ptr.Ptr Void
  -> IO FC.CInt

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_1@
-}
fun_1 ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_1_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_04318f98a3ab8d08" fun_2_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_2@
-}
fun_2 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_2_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2a7c5fa1040fa8db" fun_3_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_3@
-}
fun_3 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_3_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_810acc5cf8729d0e" fun_4_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_4@
-}
fun_4 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_4 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_4_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_83b71f7defb3b27a" fun_5_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_5@
-}
fun_5 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_5 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_5_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_62b76af3dc65da3f" fun_6_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_6@
-}
fun_6 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_6_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_100aa7fb87a5ea74" fun_7_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_7@
-}
fun_7 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_7_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cd6646babeacd609" fun_8_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_8@
-}
fun_8 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_8 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_8_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_560f1de9a83c3a6a" isSolved_base ::
     Ptr.Ptr Void
  -> IO FC.CInt

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_isSolved@
-}
isSolved ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
isSolved =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType isSolved_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ef3b85ae74bc06cf" fun_1_const_wrapper_base ::
     FC.CInt
  -> Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_1_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_1_const@
-}
fun_1_const_wrapper ::
     FC.CInt
  -> Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt
fun_1_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_1_const_wrapper_base

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h:149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_1_const =
  \x0 ->
    \x1 ->
      \x2 ->
        HsBindgen.Runtime.ConstantArray.withPtr x2 (\ptr3 ->
                                                      fun_1_const_wrapper x0 x1 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr3))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1c913685e5e76952" fun_2_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_2_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_2_const@
-}
fun_2_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt
fun_2_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_2_const_wrapper_base

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h:152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> Triplet
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_2_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_2_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_eb8daf22bd5c6f00" fun_3_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_3_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_3_const@
-}
fun_3_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt
fun_3_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_3_const_wrapper_base

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h:155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_3_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_3_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0b73e4c7695a3b2f" fun_4_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_4_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_4_const@
-}
fun_4_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt
fun_4_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_4_const_wrapper_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h:158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> List
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_4_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_4_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_374feb8086895fe3" fun_5_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_5_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_5_const@
-}
fun_5_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_5_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_5_const_wrapper_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h:161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> (HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_5_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_5_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2d1320b468c36708" fun_6_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_6_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_6_const@
-}
fun_6_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_6_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_6_const_wrapper_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h:164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> Matrix
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_6_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    fun_6_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f67f5fe5bfb57aa1" fun_7_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_7_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_7_const@
-}
fun_7_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_7_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_7_const_wrapper_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h:167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_7_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_7_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2c9356851d76320e" fun_8_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'fun_8_const'

__unique:__ @test_arraysarray_Example_Unsafe_fun_8_const@
-}
fun_8_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
fun_8_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_8_const_wrapper_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h:170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> Tripletlist
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_8_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.IncompleteArray.withPtr x1 (\ptr2 ->
                                                      fun_8_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_39b08b64fed0c5b8" isSolved_const_wrapper_base ::
     Ptr.Ptr Void
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Void
  -> IO FC.CInt

{-| Pointer-based API for 'isSolved_const'

__unique:__ @test_arraysarray_Example_Unsafe_isSolved_const@
-}
isSolved_const_wrapper ::
     Ptr.Ptr Triplet
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
  -> IO FC.CInt
isSolved_const_wrapper =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType isSolved_const_wrapper_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h:173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xss@
  -> Sudoku
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
isSolved_const =
  \x0 ->
    \x1 ->
      HsBindgen.Runtime.ConstantArray.withPtr x1 (\ptr2 ->
                                                    isSolved_const_wrapper x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_ab431ebc0519545a" fun_9_base ::
     IO (Ptr.Ptr Void)

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_9@
-}
fun_9 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
fun_9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_9_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c9ff623e6f48d3bc" fun_10_base ::
     IO (Ptr.Ptr Void)

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_10@
-}
fun_10 ::
     IO (Ptr.Ptr Triplet)
fun_10 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_10_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e714f0b7c764ba17" fun_11_base ::
     IO (Ptr.Ptr Void)

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_11@
-}
fun_11 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
fun_11 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_11_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_cc23741700ba18f7" fun_12_base ::
     IO (Ptr.Ptr Void)

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_12@
-}
fun_12 ::
     IO (Ptr.Ptr List)
fun_12 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_12_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_eb3a1364003829ac" fun_13_base ::
     IO (Ptr.Ptr Void)

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_13@
-}
fun_13 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_13 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_13_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_0f49ffbe2c13ab46" fun_14_base ::
     IO (Ptr.Ptr Void)

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_14@
-}
fun_14 ::
     IO (Ptr.Ptr Matrix)
fun_14 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_14_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_59de769fbba4ed72" fun_15_base ::
     IO (Ptr.Ptr Void)

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_15@
-}
fun_15 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_15 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_15_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1d6ecccfa4ee16ff" fun_16_base ::
     IO (Ptr.Ptr Void)

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_fun_16@
-}
fun_16 ::
     IO (Ptr.Ptr Tripletlist)
fun_16 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType fun_16_base

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6165085eab7d2806" solve_base ::
     IO (Ptr.Ptr Void)

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@

__unique:__ @test_arraysarray_Example_Unsafe_solve@
-}
solve ::
     IO (Ptr.Ptr Sudoku)
solve =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType solve_base
