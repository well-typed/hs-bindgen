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
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
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

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_1@
foreign import ccall unsafe "hs_bindgen_6d07a0b03f884547" hs_bindgen_6d07a0b03f884547 ::
     FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1 ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_1 = hs_bindgen_6d07a0b03f884547

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2@
foreign import ccall unsafe "hs_bindgen_04318f98a3ab8d08" hs_bindgen_04318f98a3ab8d08 ::
     Ptr.Ptr FC.CInt
  -> IO FC.CInt

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_2 = hs_bindgen_04318f98a3ab8d08

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3@
foreign import ccall unsafe "hs_bindgen_2a7c5fa1040fa8db" hs_bindgen_2a7c5fa1040fa8db ::
     Ptr.Ptr FC.CInt
  -> IO FC.CInt

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_3 = hs_bindgen_2a7c5fa1040fa8db

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4@
foreign import ccall unsafe "hs_bindgen_810acc5cf8729d0e" hs_bindgen_810acc5cf8729d0e ::
     Ptr.Ptr FC.CInt
  -> IO FC.CInt

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_4 = hs_bindgen_810acc5cf8729d0e

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5@
foreign import ccall unsafe "hs_bindgen_83b71f7defb3b27a" hs_bindgen_83b71f7defb3b27a ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_5 = hs_bindgen_83b71f7defb3b27a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6@
foreign import ccall unsafe "hs_bindgen_62b76af3dc65da3f" hs_bindgen_62b76af3dc65da3f ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_6 = hs_bindgen_62b76af3dc65da3f

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7@
foreign import ccall unsafe "hs_bindgen_100aa7fb87a5ea74" hs_bindgen_100aa7fb87a5ea74 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_7 = hs_bindgen_100aa7fb87a5ea74

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8@
foreign import ccall unsafe "hs_bindgen_cd6646babeacd609" hs_bindgen_cd6646babeacd609 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_8 = hs_bindgen_cd6646babeacd609

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved@
foreign import ccall unsafe "hs_bindgen_560f1de9a83c3a6a" hs_bindgen_560f1de9a83c3a6a ::
     Ptr.Ptr Triplet
  -> IO FC.CInt

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
isSolved = hs_bindgen_560f1de9a83c3a6a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_1_const@
foreign import ccall unsafe "hs_bindgen_ef3b85ae74bc06cf" hs_bindgen_ef3b85ae74bc06cf ::
     FC.CInt
  -> Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt

{-| Pointer-based API for 'fun_1_const'
-}
fun_1_const_wrapper ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_1_const_wrapper = hs_bindgen_ef3b85ae74bc06cf

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
                                                      hs_bindgen_ef3b85ae74bc06cf x0 x1 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr3))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2_const@
foreign import ccall unsafe "hs_bindgen_1c913685e5e76952" hs_bindgen_1c913685e5e76952 ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt

{-| Pointer-based API for 'fun_2_const'
-}
fun_2_const_wrapper ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_2_const_wrapper = hs_bindgen_1c913685e5e76952

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
                                                    hs_bindgen_1c913685e5e76952 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3_const@
foreign import ccall unsafe "hs_bindgen_eb8daf22bd5c6f00" hs_bindgen_eb8daf22bd5c6f00 ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt

{-| Pointer-based API for 'fun_3_const'
-}
fun_3_const_wrapper ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_3_const_wrapper = hs_bindgen_eb8daf22bd5c6f00

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
                                                      hs_bindgen_eb8daf22bd5c6f00 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4_const@
foreign import ccall unsafe "hs_bindgen_0b73e4c7695a3b2f" hs_bindgen_0b73e4c7695a3b2f ::
     Ptr.Ptr FC.CInt
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
  -> IO FC.CInt

{-| Pointer-based API for 'fun_4_const'
-}
fun_4_const_wrapper ::
     Ptr.Ptr FC.CInt
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_4_const_wrapper = hs_bindgen_0b73e4c7695a3b2f

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
                                                      hs_bindgen_0b73e4c7695a3b2f x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5_const@
foreign import ccall unsafe "hs_bindgen_374feb8086895fe3" hs_bindgen_374feb8086895fe3 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Pointer-based API for 'fun_5_const'
-}
fun_5_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_5_const_wrapper = hs_bindgen_374feb8086895fe3

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
                                                    hs_bindgen_374feb8086895fe3 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6_const@
foreign import ccall unsafe "hs_bindgen_2d1320b468c36708" hs_bindgen_2d1320b468c36708 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Pointer-based API for 'fun_6_const'
-}
fun_6_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_6_const_wrapper = hs_bindgen_2d1320b468c36708

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
                                                    hs_bindgen_2d1320b468c36708 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7_const@
foreign import ccall unsafe "hs_bindgen_f67f5fe5bfb57aa1" hs_bindgen_f67f5fe5bfb57aa1 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Pointer-based API for 'fun_7_const'
-}
fun_7_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_7_const_wrapper = hs_bindgen_f67f5fe5bfb57aa1

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
                                                      hs_bindgen_f67f5fe5bfb57aa1 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8_const@
foreign import ccall unsafe "hs_bindgen_2c9356851d76320e" hs_bindgen_2c9356851d76320e ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

{-| Pointer-based API for 'fun_8_const'
-}
fun_8_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_8_const_wrapper = hs_bindgen_2c9356851d76320e

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
                                                      hs_bindgen_2c9356851d76320e x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved_const@
foreign import ccall unsafe "hs_bindgen_39b08b64fed0c5b8" hs_bindgen_39b08b64fed0c5b8 ::
     Ptr.Ptr Triplet
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
  -> IO FC.CInt

{-| Pointer-based API for 'isSolved_const'
-}
isSolved_const_wrapper ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
isSolved_const_wrapper = hs_bindgen_39b08b64fed0c5b8

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
                                                    hs_bindgen_39b08b64fed0c5b8 x0 (HsBindgen.Runtime.ConstPtr.ConstPtr ptr2))

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_9@
foreign import ccall unsafe "hs_bindgen_ab431ebc0519545a" hs_bindgen_ab431ebc0519545a ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
fun_9 = hs_bindgen_ab431ebc0519545a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_10@
foreign import ccall unsafe "hs_bindgen_c9ff623e6f48d3bc" hs_bindgen_c9ff623e6f48d3bc ::
     IO (Ptr.Ptr Triplet)

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 :: IO (Ptr.Ptr Triplet)
fun_10 = hs_bindgen_c9ff623e6f48d3bc

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_11@
foreign import ccall unsafe "hs_bindgen_e714f0b7c764ba17" hs_bindgen_e714f0b7c764ba17 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
fun_11 = hs_bindgen_e714f0b7c764ba17

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_12@
foreign import ccall unsafe "hs_bindgen_cc23741700ba18f7" hs_bindgen_cc23741700ba18f7 ::
     IO (Ptr.Ptr List)

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 :: IO (Ptr.Ptr List)
fun_12 = hs_bindgen_cc23741700ba18f7

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_13@
foreign import ccall unsafe "hs_bindgen_eb3a1364003829ac" hs_bindgen_eb3a1364003829ac ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_13 = hs_bindgen_eb3a1364003829ac

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_14@
foreign import ccall unsafe "hs_bindgen_0f49ffbe2c13ab46" hs_bindgen_0f49ffbe2c13ab46 ::
     IO (Ptr.Ptr Matrix)

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 :: IO (Ptr.Ptr Matrix)
fun_14 = hs_bindgen_0f49ffbe2c13ab46

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_15@
foreign import ccall unsafe "hs_bindgen_59de769fbba4ed72" hs_bindgen_59de769fbba4ed72 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_15 = hs_bindgen_59de769fbba4ed72

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_16@
foreign import ccall unsafe "hs_bindgen_1d6ecccfa4ee16ff" hs_bindgen_1d6ecccfa4ee16ff ::
     IO (Ptr.Ptr Tripletlist)

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 :: IO (Ptr.Ptr Tripletlist)
fun_16 = hs_bindgen_1d6ecccfa4ee16ff

-- __unique:__ @test_arraysarray_Example_Unsafe_solve@
foreign import ccall unsafe "hs_bindgen_6165085eab7d2806" hs_bindgen_6165085eab7d2806 ::
     IO (Ptr.Ptr Sudoku)

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@
-}
solve :: IO (Ptr.Ptr Sudoku)
solve = hs_bindgen_6165085eab7d2806
