{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.HasFFIType
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.PtrConst
import Data.Void (Void)
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/array.h>"
  , "signed int hs_bindgen_a836491d63ff3a2c ("
  , "  signed int arg1,"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_1(arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_c69f41e5ccc441ab ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return fun_2(*arg1);"
  , "}"
  , "signed int hs_bindgen_30065ddbffdd7502 ("
  , "  signed int (*arg1)[]"
  , ")"
  , "{"
  , "  return fun_3(*arg1);"
  , "}"
  , "signed int hs_bindgen_6e8db8abcb5fe22a ("
  , "  list *arg1"
  , ")"
  , "{"
  , "  return fun_4(*arg1);"
  , "}"
  , "signed int hs_bindgen_b2f48c31265a3f47 ("
  , "  signed int (*arg1)[4][3]"
  , ")"
  , "{"
  , "  return fun_5(*arg1);"
  , "}"
  , "signed int hs_bindgen_343fe8ca0dbb7eb1 ("
  , "  matrix *arg1"
  , ")"
  , "{"
  , "  return fun_6(*arg1);"
  , "}"
  , "signed int hs_bindgen_d98a58d39b578fd6 ("
  , "  signed int (*arg1)[][3]"
  , ")"
  , "{"
  , "  return fun_7(*arg1);"
  , "}"
  , "signed int hs_bindgen_4db12be6f46d98f5 ("
  , "  tripletlist *arg1"
  , ")"
  , "{"
  , "  return fun_8(*arg1);"
  , "}"
  , "signed int hs_bindgen_825f9aeca071df21 ("
  , "  sudoku *arg1"
  , ")"
  , "{"
  , "  return isSolved(*arg1);"
  , "}"
  , "signed int hs_bindgen_a2bf6bc667c9e769 ("
  , "  signed int arg1,"
  , "  signed int (*arg2)[3],"
  , "  signed int const (*arg3)[3]"
  , ")"
  , "{"
  , "  return fun_1_const(arg1, *arg2, *arg3);"
  , "}"
  , "signed int hs_bindgen_ec5a6dd15a457a1d ("
  , "  triplet *arg1,"
  , "  triplet const *arg2"
  , ")"
  , "{"
  , "  return fun_2_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_84df1030280611db ("
  , "  signed int (*arg1)[],"
  , "  signed int const (*arg2)[]"
  , ")"
  , "{"
  , "  return fun_3_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_e9dc927aa39d14d3 ("
  , "  list *arg1,"
  , "  list const *arg2"
  , ")"
  , "{"
  , "  return fun_4_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_cd0bfb26f385dfaa ("
  , "  signed int (*arg1)[4][3],"
  , "  signed int const (*arg2)[4][3]"
  , ")"
  , "{"
  , "  return fun_5_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_1054ce6b48ed0f13 ("
  , "  matrix *arg1,"
  , "  matrix const *arg2"
  , ")"
  , "{"
  , "  return fun_6_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_496902d7c6466098 ("
  , "  signed int (*arg1)[][3],"
  , "  signed int const (*arg2)[][3]"
  , ")"
  , "{"
  , "  return fun_7_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_eb65cb5074167c48 ("
  , "  tripletlist *arg1,"
  , "  tripletlist const *arg2"
  , ")"
  , "{"
  , "  return fun_8_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_9bb064e9eddf07f7 ("
  , "  sudoku *arg1,"
  , "  sudoku const *arg2"
  , ")"
  , "{"
  , "  return isSolved_const(*arg1, *arg2);"
  , "}"
  , "signed int (*hs_bindgen_0fa0a3e47fa9d95a (void))[3]"
  , "{"
  , "  return fun_9();"
  , "}"
  , "triplet *hs_bindgen_e9d3d35727502125 (void)"
  , "{"
  , "  return fun_10();"
  , "}"
  , "signed int (*hs_bindgen_7f7cea54b33bf176 (void))[]"
  , "{"
  , "  return fun_11();"
  , "}"
  , "list *hs_bindgen_3124a96b00bbc082 (void)"
  , "{"
  , "  return fun_12();"
  , "}"
  , "signed int (*hs_bindgen_2fdd1bf9ee687f9b (void))[4][3]"
  , "{"
  , "  return fun_13();"
  , "}"
  , "matrix *hs_bindgen_12a242894a3d45cb (void)"
  , "{"
  , "  return fun_14();"
  , "}"
  , "signed int (*hs_bindgen_d8e176eb5efefa2c (void))[][3]"
  , "{"
  , "  return fun_15();"
  , "}"
  , "tripletlist *hs_bindgen_dcf234ca786626c7 (void)"
  , "{"
  , "  return fun_16();"
  , "}"
  , "sudoku *hs_bindgen_f80a5b6a2770c658 (void)"
  , "{"
  , "  return solve();"
  , "}"
  ]))

-- __unique:__ @test_arraysarray_Example_Safe_fun_1@
foreign import ccall safe "hs_bindgen_a836491d63ff3a2c" hs_bindgen_a836491d63ff3a2c_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_1@
hs_bindgen_a836491d63ff3a2c ::
     FC.CInt
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_a836491d63ff3a2c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a836491d63ff3a2c_base

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h 118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1 ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_1 = hs_bindgen_a836491d63ff3a2c

-- __unique:__ @test_arraysarray_Example_Safe_fun_2@
foreign import ccall safe "hs_bindgen_c69f41e5ccc441ab" hs_bindgen_c69f41e5ccc441ab_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_2@
hs_bindgen_c69f41e5ccc441ab ::
     Ptr.Ptr Triplet
  -> IO FC.CInt
hs_bindgen_c69f41e5ccc441ab =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_c69f41e5ccc441ab_base

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h 121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_2 = hs_bindgen_c69f41e5ccc441ab

-- __unique:__ @test_arraysarray_Example_Safe_fun_3@
foreign import ccall safe "hs_bindgen_30065ddbffdd7502" hs_bindgen_30065ddbffdd7502_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_3@
hs_bindgen_30065ddbffdd7502 ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
  -> IO FC.CInt
hs_bindgen_30065ddbffdd7502 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_30065ddbffdd7502_base

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h 124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_3 = hs_bindgen_30065ddbffdd7502

-- __unique:__ @test_arraysarray_Example_Safe_fun_4@
foreign import ccall safe "hs_bindgen_6e8db8abcb5fe22a" hs_bindgen_6e8db8abcb5fe22a_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_4@
hs_bindgen_6e8db8abcb5fe22a ::
     Ptr.Ptr List
  -> IO FC.CInt
hs_bindgen_6e8db8abcb5fe22a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_6e8db8abcb5fe22a_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h 127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 ::
     Ptr.Ptr List
     -- ^ __C declaration:__ @xs@
  -> IO FC.CInt
fun_4 = hs_bindgen_6e8db8abcb5fe22a

-- __unique:__ @test_arraysarray_Example_Safe_fun_5@
foreign import ccall safe "hs_bindgen_b2f48c31265a3f47" hs_bindgen_b2f48c31265a3f47_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_5@
hs_bindgen_b2f48c31265a3f47 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> IO FC.CInt
hs_bindgen_b2f48c31265a3f47 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_b2f48c31265a3f47_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h 130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_5 = hs_bindgen_b2f48c31265a3f47

-- __unique:__ @test_arraysarray_Example_Safe_fun_6@
foreign import ccall safe "hs_bindgen_343fe8ca0dbb7eb1" hs_bindgen_343fe8ca0dbb7eb1_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_6@
hs_bindgen_343fe8ca0dbb7eb1 ::
     Ptr.Ptr Matrix
  -> IO FC.CInt
hs_bindgen_343fe8ca0dbb7eb1 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_343fe8ca0dbb7eb1_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h 133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 ::
     Ptr.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_6 = hs_bindgen_343fe8ca0dbb7eb1

-- __unique:__ @test_arraysarray_Example_Safe_fun_7@
foreign import ccall safe "hs_bindgen_d98a58d39b578fd6" hs_bindgen_d98a58d39b578fd6_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_7@
hs_bindgen_d98a58d39b578fd6 ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> IO FC.CInt
hs_bindgen_d98a58d39b578fd6 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d98a58d39b578fd6_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h 136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_7 = hs_bindgen_d98a58d39b578fd6

-- __unique:__ @test_arraysarray_Example_Safe_fun_8@
foreign import ccall safe "hs_bindgen_4db12be6f46d98f5" hs_bindgen_4db12be6f46d98f5_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_8@
hs_bindgen_4db12be6f46d98f5 ::
     Ptr.Ptr Tripletlist
  -> IO FC.CInt
hs_bindgen_4db12be6f46d98f5 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_4db12be6f46d98f5_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h 139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 ::
     Ptr.Ptr Tripletlist
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
fun_8 = hs_bindgen_4db12be6f46d98f5

-- __unique:__ @test_arraysarray_Example_Safe_isSolved@
foreign import ccall safe "hs_bindgen_825f9aeca071df21" hs_bindgen_825f9aeca071df21_base ::
     Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_isSolved@
hs_bindgen_825f9aeca071df21 ::
     Ptr.Ptr Sudoku
  -> IO FC.CInt
hs_bindgen_825f9aeca071df21 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_825f9aeca071df21_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h 142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved ::
     Ptr.Ptr Sudoku
     -- ^ __C declaration:__ @xss@
  -> IO FC.CInt
isSolved = hs_bindgen_825f9aeca071df21

-- __unique:__ @test_arraysarray_Example_Safe_fun_1_const@
foreign import ccall safe "hs_bindgen_a2bf6bc667c9e769" hs_bindgen_a2bf6bc667c9e769_base ::
     GHC.Int.Int32
  -> Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_1_const@
hs_bindgen_a2bf6bc667c9e769 ::
     FC.CInt
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
  -> IO FC.CInt
hs_bindgen_a2bf6bc667c9e769 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_a2bf6bc667c9e769_base

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h 149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const ::
     FC.CInt
     -- ^ __C declaration:__ @x@
  -> Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_1_const = hs_bindgen_a2bf6bc667c9e769

-- __unique:__ @test_arraysarray_Example_Safe_fun_2_const@
foreign import ccall safe "hs_bindgen_ec5a6dd15a457a1d" hs_bindgen_ec5a6dd15a457a1d_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_2_const@
hs_bindgen_ec5a6dd15a457a1d ::
     Ptr.Ptr Triplet
  -> HsBindgen.Runtime.PtrConst.PtrConst Triplet
  -> IO FC.CInt
hs_bindgen_ec5a6dd15a457a1d =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_ec5a6dd15a457a1d_base

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h 152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const ::
     Ptr.Ptr Triplet
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.PtrConst.PtrConst Triplet
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_2_const = hs_bindgen_ec5a6dd15a457a1d

-- __unique:__ @test_arraysarray_Example_Safe_fun_3_const@
foreign import ccall safe "hs_bindgen_84df1030280611db" hs_bindgen_84df1030280611db_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_3_const@
hs_bindgen_84df1030280611db ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
  -> HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
  -> IO FC.CInt
hs_bindgen_84df1030280611db =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_84df1030280611db_base

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h 155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_3_const = hs_bindgen_84df1030280611db

-- __unique:__ @test_arraysarray_Example_Safe_fun_4_const@
foreign import ccall safe "hs_bindgen_e9dc927aa39d14d3" hs_bindgen_e9dc927aa39d14d3_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_4_const@
hs_bindgen_e9dc927aa39d14d3 ::
     Ptr.Ptr List
  -> HsBindgen.Runtime.PtrConst.PtrConst List
  -> IO FC.CInt
hs_bindgen_e9dc927aa39d14d3 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e9dc927aa39d14d3_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h 158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const ::
     Ptr.Ptr List
     -- ^ __C declaration:__ @xs@
  -> HsBindgen.Runtime.PtrConst.PtrConst List
     -- ^ __C declaration:__ @ys@
  -> IO FC.CInt
fun_4_const = hs_bindgen_e9dc927aa39d14d3

-- __unique:__ @test_arraysarray_Example_Safe_fun_5_const@
foreign import ccall safe "hs_bindgen_cd0bfb26f385dfaa" hs_bindgen_cd0bfb26f385dfaa_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_5_const@
hs_bindgen_cd0bfb26f385dfaa ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> IO FC.CInt
hs_bindgen_cd0bfb26f385dfaa =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_cd0bfb26f385dfaa_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h 161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const ::
     Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.PtrConst.PtrConst ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_5_const = hs_bindgen_cd0bfb26f385dfaa

-- __unique:__ @test_arraysarray_Example_Safe_fun_6_const@
foreign import ccall safe "hs_bindgen_1054ce6b48ed0f13" hs_bindgen_1054ce6b48ed0f13_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_6_const@
hs_bindgen_1054ce6b48ed0f13 ::
     Ptr.Ptr Matrix
  -> HsBindgen.Runtime.PtrConst.PtrConst Matrix
  -> IO FC.CInt
hs_bindgen_1054ce6b48ed0f13 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_1054ce6b48ed0f13_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h 164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const ::
     Ptr.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_6_const = hs_bindgen_1054ce6b48ed0f13

-- __unique:__ @test_arraysarray_Example_Safe_fun_7_const@
foreign import ccall safe "hs_bindgen_496902d7c6466098" hs_bindgen_496902d7c6466098_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_7_const@
hs_bindgen_496902d7c6466098 ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
  -> IO FC.CInt
hs_bindgen_496902d7c6466098 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_496902d7c6466098_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h 167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const ::
     Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.PtrConst.PtrConst (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_7_const = hs_bindgen_496902d7c6466098

-- __unique:__ @test_arraysarray_Example_Safe_fun_8_const@
foreign import ccall safe "hs_bindgen_eb65cb5074167c48" hs_bindgen_eb65cb5074167c48_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_8_const@
hs_bindgen_eb65cb5074167c48 ::
     Ptr.Ptr Tripletlist
  -> HsBindgen.Runtime.PtrConst.PtrConst Tripletlist
  -> IO FC.CInt
hs_bindgen_eb65cb5074167c48 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_eb65cb5074167c48_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h 170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const ::
     Ptr.Ptr Tripletlist
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.PtrConst.PtrConst Tripletlist
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
fun_8_const = hs_bindgen_eb65cb5074167c48

-- __unique:__ @test_arraysarray_Example_Safe_isSolved_const@
foreign import ccall safe "hs_bindgen_9bb064e9eddf07f7" hs_bindgen_9bb064e9eddf07f7_base ::
     Ptr.Ptr Void
  -> Ptr.Ptr Void
  -> IO GHC.Int.Int32

-- __unique:__ @test_arraysarray_Example_Safe_isSolved_const@
hs_bindgen_9bb064e9eddf07f7 ::
     Ptr.Ptr Sudoku
  -> HsBindgen.Runtime.PtrConst.PtrConst Sudoku
  -> IO FC.CInt
hs_bindgen_9bb064e9eddf07f7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_9bb064e9eddf07f7_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h 173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const ::
     Ptr.Ptr Sudoku
     -- ^ __C declaration:__ @xss@
  -> HsBindgen.Runtime.PtrConst.PtrConst Sudoku
     -- ^ __C declaration:__ @yss@
  -> IO FC.CInt
isSolved_const = hs_bindgen_9bb064e9eddf07f7

-- __unique:__ @test_arraysarray_Example_Safe_fun_9@
foreign import ccall safe "hs_bindgen_0fa0a3e47fa9d95a" hs_bindgen_0fa0a3e47fa9d95a_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_9@
hs_bindgen_0fa0a3e47fa9d95a :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
hs_bindgen_0fa0a3e47fa9d95a =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_0fa0a3e47fa9d95a_base

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h 185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt))
fun_9 = hs_bindgen_0fa0a3e47fa9d95a

-- __unique:__ @test_arraysarray_Example_Safe_fun_10@
foreign import ccall safe "hs_bindgen_e9d3d35727502125" hs_bindgen_e9d3d35727502125_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_10@
hs_bindgen_e9d3d35727502125 :: IO (Ptr.Ptr Triplet)
hs_bindgen_e9d3d35727502125 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_e9d3d35727502125_base

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h 188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 :: IO (Ptr.Ptr Triplet)
fun_10 = hs_bindgen_e9d3d35727502125

-- __unique:__ @test_arraysarray_Example_Safe_fun_11@
foreign import ccall safe "hs_bindgen_7f7cea54b33bf176" hs_bindgen_7f7cea54b33bf176_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_11@
hs_bindgen_7f7cea54b33bf176 :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
hs_bindgen_7f7cea54b33bf176 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_7f7cea54b33bf176_base

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h 191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
fun_11 = hs_bindgen_7f7cea54b33bf176

-- __unique:__ @test_arraysarray_Example_Safe_fun_12@
foreign import ccall safe "hs_bindgen_3124a96b00bbc082" hs_bindgen_3124a96b00bbc082_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_12@
hs_bindgen_3124a96b00bbc082 :: IO (Ptr.Ptr List)
hs_bindgen_3124a96b00bbc082 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_3124a96b00bbc082_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h 194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 :: IO (Ptr.Ptr List)
fun_12 = hs_bindgen_3124a96b00bbc082

-- __unique:__ @test_arraysarray_Example_Safe_fun_13@
foreign import ccall safe "hs_bindgen_2fdd1bf9ee687f9b" hs_bindgen_2fdd1bf9ee687f9b_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_13@
hs_bindgen_2fdd1bf9ee687f9b :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_2fdd1bf9ee687f9b =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_2fdd1bf9ee687f9b_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h 197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_13 = hs_bindgen_2fdd1bf9ee687f9b

-- __unique:__ @test_arraysarray_Example_Safe_fun_14@
foreign import ccall safe "hs_bindgen_12a242894a3d45cb" hs_bindgen_12a242894a3d45cb_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_14@
hs_bindgen_12a242894a3d45cb :: IO (Ptr.Ptr Matrix)
hs_bindgen_12a242894a3d45cb =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_12a242894a3d45cb_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h 200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 :: IO (Ptr.Ptr Matrix)
fun_14 = hs_bindgen_12a242894a3d45cb

-- __unique:__ @test_arraysarray_Example_Safe_fun_15@
foreign import ccall safe "hs_bindgen_d8e176eb5efefa2c" hs_bindgen_d8e176eb5efefa2c_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_15@
hs_bindgen_d8e176eb5efefa2c :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
hs_bindgen_d8e176eb5efefa2c =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_d8e176eb5efefa2c_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h 203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 :: IO (Ptr.Ptr (HsBindgen.Runtime.IncompleteArray.IncompleteArray ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt)))
fun_15 = hs_bindgen_d8e176eb5efefa2c

-- __unique:__ @test_arraysarray_Example_Safe_fun_16@
foreign import ccall safe "hs_bindgen_dcf234ca786626c7" hs_bindgen_dcf234ca786626c7_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_16@
hs_bindgen_dcf234ca786626c7 :: IO (Ptr.Ptr Tripletlist)
hs_bindgen_dcf234ca786626c7 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_dcf234ca786626c7_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h 206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 :: IO (Ptr.Ptr Tripletlist)
fun_16 = hs_bindgen_dcf234ca786626c7

-- __unique:__ @test_arraysarray_Example_Safe_solve@
foreign import ccall safe "hs_bindgen_f80a5b6a2770c658" hs_bindgen_f80a5b6a2770c658_base ::
     IO (Ptr.Ptr Void)

-- __unique:__ @test_arraysarray_Example_Safe_solve@
hs_bindgen_f80a5b6a2770c658 :: IO (Ptr.Ptr Sudoku)
hs_bindgen_f80a5b6a2770c658 =
  HsBindgen.Runtime.HasFFIType.fromFFIType hs_bindgen_f80a5b6a2770c658_base

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h 209:10@

__exported by:__ @arrays\/array.h@
-}
solve :: IO (Ptr.Ptr Sudoku)
solve = hs_bindgen_f80a5b6a2770c658
