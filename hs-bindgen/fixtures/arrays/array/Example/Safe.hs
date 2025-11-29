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
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <arrays/array.h>"
  , "signed int hs_bindgen_test_arraysarray_b4e5862c015bb24a ("
  , "  signed int arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_1(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_e0987ef0efc4a453 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_2(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_36b192f126f8cb38 ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_3(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_54447aefe7d3cd2c ("
  , "  signed int *arg1"
  , ")"
  , "{"
  , "  return fun_4(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_e8ba27c60a1903bd ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_5(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_0607040f11a12782 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_6(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_006a166f3186f949 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_7(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_fb5dfc9e181645a9 ("
  , "  signed int (*arg1)[3]"
  , ")"
  , "{"
  , "  return fun_8(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_5eb9545ce77d269b ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return isSolved(arg1);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_ca60a29d37039596 ("
  , "  signed int arg1,"
  , "  signed int *arg2,"
  , "  signed int const *arg3"
  , ")"
  , "{"
  , "  return fun_1_const(arg1, arg2, arg3);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_bbca5e34f321cd74 ("
  , "  signed int *arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_2_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_240b1c3315ee4211 ("
  , "  signed int *arg1,"
  , "  signed int const *arg2"
  , ")"
  , "{"
  , "  return fun_3_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_6632b7edaabe1cc0 ("
  , "  signed int *arg1,"
  , "  signed int *arg2"
  , ")"
  , "{"
  , "  return fun_4_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_8a8844c01ba46620 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_5_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_edc64ebd2895dcf7 ("
  , "  signed int (*arg1)[3],"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_6_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_653f331248c9d935 ("
  , "  signed int (*arg1)[3],"
  , "  signed int const (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_7_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_711734948439605e ("
  , "  signed int (*arg1)[3],"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_8_const(arg1, arg2);"
  , "}"
  , "signed int hs_bindgen_test_arraysarray_13fc5478d98dcf7e ("
  , "  triplet *arg1,"
  , "  triplet *arg2"
  , ")"
  , "{"
  , "  return isSolved_const(arg1, arg2);"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_44a19e49a4c13fe6 (void))[3]"
  , "{"
  , "  return fun_9();"
  , "}"
  , "triplet *hs_bindgen_test_arraysarray_4ec40d1a47a19a8b (void)"
  , "{"
  , "  return fun_10();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_96ba4ddc37567d90 (void))[]"
  , "{"
  , "  return fun_11();"
  , "}"
  , "list *hs_bindgen_test_arraysarray_061e07c5a07d8379 (void)"
  , "{"
  , "  return fun_12();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_11bc2ac8e6b93fbd (void))[4][3]"
  , "{"
  , "  return fun_13();"
  , "}"
  , "matrix *hs_bindgen_test_arraysarray_7982c7ae26595405 (void)"
  , "{"
  , "  return fun_14();"
  , "}"
  , "signed int (*hs_bindgen_test_arraysarray_63ff263403abf302 (void))[][3]"
  , "{"
  , "  return fun_15();"
  , "}"
  , "tripletlist *hs_bindgen_test_arraysarray_a07ddfea70770931 (void)"
  , "{"
  , "  return fun_16();"
  , "}"
  , "sudoku *hs_bindgen_test_arraysarray_45cc9e5cdeba22d6 (void)"
  , "{"
  , "  return solve();"
  , "}"
  ]))

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h:118:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_1@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_b4e5862c015bb24a" fun_1 ::
     FC.CInt
     {- ^ __C declaration:__ @x@
     -}
  -> Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h:121:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_2@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_e0987ef0efc4a453" fun_2 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h:124:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_3@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_36b192f126f8cb38" fun_3 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h:127:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_4@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_54447aefe7d3cd2c" fun_4 ::
     Ptr.Ptr FC.CInt
     {- ^ __C declaration:__ @xs@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h:130:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_5@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_e8ba27c60a1903bd" fun_5 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h:133:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_6@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_0607040f11a12782" fun_6 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h:136:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_7@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_006a166f3186f949" fun_7 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h:139:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_8@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_fb5dfc9e181645a9" fun_8 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h:142:5@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_isSolved@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_5eb9545ce77d269b" isSolved ::
     Ptr.Ptr Triplet
     {- ^ __C declaration:__ @xss@
     -}
  -> IO FC.CInt

{-| Pointer-based API for 'fun_1_const'

__unique:__ @Example_Safe_fun_1_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_ca60a29d37039596" fun_1_const_wrapper ::
     FC.CInt
  -> Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_2_const'

__unique:__ @Example_Safe_fun_2_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_bbca5e34f321cd74" fun_2_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_3_const'

__unique:__ @Example_Safe_fun_3_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_240b1c3315ee4211" fun_3_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_4_const'

__unique:__ @Example_Safe_fun_4_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_6632b7edaabe1cc0" fun_4_const_wrapper ::
     Ptr.Ptr FC.CInt
  -> Ptr.Ptr FC.CInt
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_5_const'

__unique:__ @Example_Safe_fun_5_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_8a8844c01ba46620" fun_5_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_6_const'

__unique:__ @Example_Safe_fun_6_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_edc64ebd2895dcf7" fun_6_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_7_const'

__unique:__ @Example_Safe_fun_7_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_653f331248c9d935" fun_7_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

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

{-| Pointer-based API for 'fun_8_const'

__unique:__ @Example_Safe_fun_8_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_711734948439605e" fun_8_const_wrapper ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt

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

{-| Pointer-based API for 'isSolved_const'

__unique:__ @Example_Safe_isSolved_const@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_13fc5478d98dcf7e" isSolved_const_wrapper ::
     Ptr.Ptr Triplet
  -> Ptr.Ptr Triplet
  -> IO FC.CInt

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

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h:185:7@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_9@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_44a19e49a4c13fe6" fun_9 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h:188:10@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_10@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_4ec40d1a47a19a8b" fun_10 ::
     IO (Ptr.Ptr Triplet)

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h:191:7@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_11@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_96ba4ddc37567d90" fun_11 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h:194:7@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_12@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_061e07c5a07d8379" fun_12 ::
     IO (Ptr.Ptr List)

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h:197:7@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_13@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_11bc2ac8e6b93fbd" fun_13 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h:200:9@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_14@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_7982c7ae26595405" fun_14 ::
     IO (Ptr.Ptr Matrix)

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h:203:7@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_15@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_63ff263403abf302" fun_15 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h:206:14@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_fun_16@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_a07ddfea70770931" fun_16 ::
     IO (Ptr.Ptr Tripletlist)

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h:209:10@

__exported by:__ @arrays\/array.h@

__unique:__ @Example_Safe_solve@
-}
foreign import ccall safe "hs_bindgen_test_arraysarray_45cc9e5cdeba22d6" solve ::
     IO (Ptr.Ptr Sudoku)
