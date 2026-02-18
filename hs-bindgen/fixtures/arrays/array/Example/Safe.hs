{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Safe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

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
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_1@
hs_bindgen_a836491d63ff3a2c ::
     RIP.CInt
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_a836491d63ff3a2c =
  RIP.fromFFIType hs_bindgen_a836491d63ff3a2c_base

{-| Array of known size

__C declaration:__ @fun_1@

__defined at:__ @arrays\/array.h 118:5@

__exported by:__ @arrays\/array.h@
-}
fun_1 ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_1 = hs_bindgen_a836491d63ff3a2c

-- __unique:__ @test_arraysarray_Example_Safe_fun_2@
foreign import ccall safe "hs_bindgen_c69f41e5ccc441ab" hs_bindgen_c69f41e5ccc441ab_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_2@
hs_bindgen_c69f41e5ccc441ab ::
     RIP.Ptr Triplet
  -> IO RIP.CInt
hs_bindgen_c69f41e5ccc441ab =
  RIP.fromFFIType hs_bindgen_c69f41e5ccc441ab_base

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h 121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 ::
     RIP.Ptr Triplet
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_2 = hs_bindgen_c69f41e5ccc441ab

-- __unique:__ @test_arraysarray_Example_Safe_fun_3@
foreign import ccall safe "hs_bindgen_30065ddbffdd7502" hs_bindgen_30065ddbffdd7502_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_3@
hs_bindgen_30065ddbffdd7502 ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_30065ddbffdd7502 =
  RIP.fromFFIType hs_bindgen_30065ddbffdd7502_base

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h 124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_3 = hs_bindgen_30065ddbffdd7502

-- __unique:__ @test_arraysarray_Example_Safe_fun_4@
foreign import ccall safe "hs_bindgen_6e8db8abcb5fe22a" hs_bindgen_6e8db8abcb5fe22a_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_4@
hs_bindgen_6e8db8abcb5fe22a ::
     RIP.Ptr List
  -> IO RIP.CInt
hs_bindgen_6e8db8abcb5fe22a =
  RIP.fromFFIType hs_bindgen_6e8db8abcb5fe22a_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h 127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 ::
     RIP.Ptr List
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_4 = hs_bindgen_6e8db8abcb5fe22a

-- __unique:__ @test_arraysarray_Example_Safe_fun_5@
foreign import ccall safe "hs_bindgen_b2f48c31265a3f47" hs_bindgen_b2f48c31265a3f47_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_5@
hs_bindgen_b2f48c31265a3f47 ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_b2f48c31265a3f47 =
  RIP.fromFFIType hs_bindgen_b2f48c31265a3f47_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h 130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_5 = hs_bindgen_b2f48c31265a3f47

-- __unique:__ @test_arraysarray_Example_Safe_fun_6@
foreign import ccall safe "hs_bindgen_343fe8ca0dbb7eb1" hs_bindgen_343fe8ca0dbb7eb1_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_6@
hs_bindgen_343fe8ca0dbb7eb1 ::
     RIP.Ptr Matrix
  -> IO RIP.CInt
hs_bindgen_343fe8ca0dbb7eb1 =
  RIP.fromFFIType hs_bindgen_343fe8ca0dbb7eb1_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h 133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 ::
     RIP.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_6 = hs_bindgen_343fe8ca0dbb7eb1

-- __unique:__ @test_arraysarray_Example_Safe_fun_7@
foreign import ccall safe "hs_bindgen_d98a58d39b578fd6" hs_bindgen_d98a58d39b578fd6_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_7@
hs_bindgen_d98a58d39b578fd6 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_d98a58d39b578fd6 =
  RIP.fromFFIType hs_bindgen_d98a58d39b578fd6_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h 136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_7 = hs_bindgen_d98a58d39b578fd6

-- __unique:__ @test_arraysarray_Example_Safe_fun_8@
foreign import ccall safe "hs_bindgen_4db12be6f46d98f5" hs_bindgen_4db12be6f46d98f5_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_8@
hs_bindgen_4db12be6f46d98f5 ::
     RIP.Ptr Tripletlist
  -> IO RIP.CInt
hs_bindgen_4db12be6f46d98f5 =
  RIP.fromFFIType hs_bindgen_4db12be6f46d98f5_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h 139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 ::
     RIP.Ptr Tripletlist
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_8 = hs_bindgen_4db12be6f46d98f5

-- __unique:__ @test_arraysarray_Example_Safe_isSolved@
foreign import ccall safe "hs_bindgen_825f9aeca071df21" hs_bindgen_825f9aeca071df21_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_isSolved@
hs_bindgen_825f9aeca071df21 ::
     RIP.Ptr Sudoku
  -> IO RIP.CInt
hs_bindgen_825f9aeca071df21 =
  RIP.fromFFIType hs_bindgen_825f9aeca071df21_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h 142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved ::
     RIP.Ptr Sudoku
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
isSolved = hs_bindgen_825f9aeca071df21

-- __unique:__ @test_arraysarray_Example_Safe_fun_1_const@
foreign import ccall safe "hs_bindgen_a2bf6bc667c9e769" hs_bindgen_a2bf6bc667c9e769_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_1_const@
hs_bindgen_a2bf6bc667c9e769 ::
     RIP.CInt
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
  -> PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_a2bf6bc667c9e769 =
  RIP.fromFFIType hs_bindgen_a2bf6bc667c9e769_base

{-| Array of known size

__C declaration:__ @fun_1_const@

__defined at:__ @arrays\/array.h 149:5@

__exported by:__ @arrays\/array.h@
-}
fun_1_const ::
     RIP.CInt
     -- ^ __C declaration:__ @x@
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @xs@
  -> PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
     -- ^ __C declaration:__ @ys@
  -> IO RIP.CInt
fun_1_const = hs_bindgen_a2bf6bc667c9e769

-- __unique:__ @test_arraysarray_Example_Safe_fun_2_const@
foreign import ccall safe "hs_bindgen_ec5a6dd15a457a1d" hs_bindgen_ec5a6dd15a457a1d_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_2_const@
hs_bindgen_ec5a6dd15a457a1d ::
     RIP.Ptr Triplet
  -> PtrConst.PtrConst Triplet
  -> IO RIP.CInt
hs_bindgen_ec5a6dd15a457a1d =
  RIP.fromFFIType hs_bindgen_ec5a6dd15a457a1d_base

{-| Array of known size, typedef

__C declaration:__ @fun_2_const@

__defined at:__ @arrays\/array.h 152:5@

__exported by:__ @arrays\/array.h@
-}
fun_2_const ::
     RIP.Ptr Triplet
     -- ^ __C declaration:__ @xs@
  -> PtrConst.PtrConst Triplet
     -- ^ __C declaration:__ @ys@
  -> IO RIP.CInt
fun_2_const = hs_bindgen_ec5a6dd15a457a1d

-- __unique:__ @test_arraysarray_Example_Safe_fun_3_const@
foreign import ccall safe "hs_bindgen_84df1030280611db" hs_bindgen_84df1030280611db_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_3_const@
hs_bindgen_84df1030280611db ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
  -> PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_84df1030280611db =
  RIP.fromFFIType hs_bindgen_84df1030280611db_base

{-| Array of unknown size

__C declaration:__ @fun_3_const@

__defined at:__ @arrays\/array.h 155:5@

__exported by:__ @arrays\/array.h@
-}
fun_3_const ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @xs@
  -> PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @ys@
  -> IO RIP.CInt
fun_3_const = hs_bindgen_84df1030280611db

-- __unique:__ @test_arraysarray_Example_Safe_fun_4_const@
foreign import ccall safe "hs_bindgen_e9dc927aa39d14d3" hs_bindgen_e9dc927aa39d14d3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_4_const@
hs_bindgen_e9dc927aa39d14d3 ::
     RIP.Ptr List
  -> PtrConst.PtrConst List
  -> IO RIP.CInt
hs_bindgen_e9dc927aa39d14d3 =
  RIP.fromFFIType hs_bindgen_e9dc927aa39d14d3_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4_const@

__defined at:__ @arrays\/array.h 158:5@

__exported by:__ @arrays\/array.h@
-}
fun_4_const ::
     RIP.Ptr List
     -- ^ __C declaration:__ @xs@
  -> PtrConst.PtrConst List
     -- ^ __C declaration:__ @ys@
  -> IO RIP.CInt
fun_4_const = hs_bindgen_e9dc927aa39d14d3

-- __unique:__ @test_arraysarray_Example_Safe_fun_5_const@
foreign import ccall safe "hs_bindgen_cd0bfb26f385dfaa" hs_bindgen_cd0bfb26f385dfaa_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_5_const@
hs_bindgen_cd0bfb26f385dfaa ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> PtrConst.PtrConst ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_cd0bfb26f385dfaa =
  RIP.fromFFIType hs_bindgen_cd0bfb26f385dfaa_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5_const@

__defined at:__ @arrays\/array.h 161:5@

__exported by:__ @arrays\/array.h@
-}
fun_5_const ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> PtrConst.PtrConst ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @yss@
  -> IO RIP.CInt
fun_5_const = hs_bindgen_cd0bfb26f385dfaa

-- __unique:__ @test_arraysarray_Example_Safe_fun_6_const@
foreign import ccall safe "hs_bindgen_1054ce6b48ed0f13" hs_bindgen_1054ce6b48ed0f13_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_6_const@
hs_bindgen_1054ce6b48ed0f13 ::
     RIP.Ptr Matrix
  -> PtrConst.PtrConst Matrix
  -> IO RIP.CInt
hs_bindgen_1054ce6b48ed0f13 =
  RIP.fromFFIType hs_bindgen_1054ce6b48ed0f13_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6_const@

__defined at:__ @arrays\/array.h 164:5@

__exported by:__ @arrays\/array.h@
-}
fun_6_const ::
     RIP.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> PtrConst.PtrConst Matrix
     -- ^ __C declaration:__ @yss@
  -> IO RIP.CInt
fun_6_const = hs_bindgen_1054ce6b48ed0f13

-- __unique:__ @test_arraysarray_Example_Safe_fun_7_const@
foreign import ccall safe "hs_bindgen_496902d7c6466098" hs_bindgen_496902d7c6466098_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_7_const@
hs_bindgen_496902d7c6466098 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_496902d7c6466098 =
  RIP.fromFFIType hs_bindgen_496902d7c6466098_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7_const@

__defined at:__ @arrays\/array.h 167:5@

__exported by:__ @arrays\/array.h@
-}
fun_7_const ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @yss@
  -> IO RIP.CInt
fun_7_const = hs_bindgen_496902d7c6466098

-- __unique:__ @test_arraysarray_Example_Safe_fun_8_const@
foreign import ccall safe "hs_bindgen_eb65cb5074167c48" hs_bindgen_eb65cb5074167c48_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_fun_8_const@
hs_bindgen_eb65cb5074167c48 ::
     RIP.Ptr Tripletlist
  -> PtrConst.PtrConst Tripletlist
  -> IO RIP.CInt
hs_bindgen_eb65cb5074167c48 =
  RIP.fromFFIType hs_bindgen_eb65cb5074167c48_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8_const@

__defined at:__ @arrays\/array.h 170:5@

__exported by:__ @arrays\/array.h@
-}
fun_8_const ::
     RIP.Ptr Tripletlist
     -- ^ __C declaration:__ @xss@
  -> PtrConst.PtrConst Tripletlist
     -- ^ __C declaration:__ @yss@
  -> IO RIP.CInt
fun_8_const = hs_bindgen_eb65cb5074167c48

-- __unique:__ @test_arraysarray_Example_Safe_isSolved_const@
foreign import ccall safe "hs_bindgen_9bb064e9eddf07f7" hs_bindgen_9bb064e9eddf07f7_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Safe_isSolved_const@
hs_bindgen_9bb064e9eddf07f7 ::
     RIP.Ptr Sudoku
  -> PtrConst.PtrConst Sudoku
  -> IO RIP.CInt
hs_bindgen_9bb064e9eddf07f7 =
  RIP.fromFFIType hs_bindgen_9bb064e9eddf07f7_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved_const@

__defined at:__ @arrays\/array.h 173:5@

__exported by:__ @arrays\/array.h@
-}
isSolved_const ::
     RIP.Ptr Sudoku
     -- ^ __C declaration:__ @xss@
  -> PtrConst.PtrConst Sudoku
     -- ^ __C declaration:__ @yss@
  -> IO RIP.CInt
isSolved_const = hs_bindgen_9bb064e9eddf07f7

-- __unique:__ @test_arraysarray_Example_Safe_fun_9@
foreign import ccall safe "hs_bindgen_0fa0a3e47fa9d95a" hs_bindgen_0fa0a3e47fa9d95a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_9@
hs_bindgen_0fa0a3e47fa9d95a :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_0fa0a3e47fa9d95a =
  RIP.fromFFIType hs_bindgen_0fa0a3e47fa9d95a_base

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h 185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
fun_9 = hs_bindgen_0fa0a3e47fa9d95a

-- __unique:__ @test_arraysarray_Example_Safe_fun_10@
foreign import ccall safe "hs_bindgen_e9d3d35727502125" hs_bindgen_e9d3d35727502125_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_10@
hs_bindgen_e9d3d35727502125 :: IO (RIP.Ptr Triplet)
hs_bindgen_e9d3d35727502125 =
  RIP.fromFFIType hs_bindgen_e9d3d35727502125_base

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h 188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 :: IO (RIP.Ptr Triplet)
fun_10 = hs_bindgen_e9d3d35727502125

-- __unique:__ @test_arraysarray_Example_Safe_fun_11@
foreign import ccall safe "hs_bindgen_7f7cea54b33bf176" hs_bindgen_7f7cea54b33bf176_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_11@
hs_bindgen_7f7cea54b33bf176 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
hs_bindgen_7f7cea54b33bf176 =
  RIP.fromFFIType hs_bindgen_7f7cea54b33bf176_base

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h 191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
fun_11 = hs_bindgen_7f7cea54b33bf176

-- __unique:__ @test_arraysarray_Example_Safe_fun_12@
foreign import ccall safe "hs_bindgen_3124a96b00bbc082" hs_bindgen_3124a96b00bbc082_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_12@
hs_bindgen_3124a96b00bbc082 :: IO (RIP.Ptr List)
hs_bindgen_3124a96b00bbc082 =
  RIP.fromFFIType hs_bindgen_3124a96b00bbc082_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h 194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 :: IO (RIP.Ptr List)
fun_12 = hs_bindgen_3124a96b00bbc082

-- __unique:__ @test_arraysarray_Example_Safe_fun_13@
foreign import ccall safe "hs_bindgen_2fdd1bf9ee687f9b" hs_bindgen_2fdd1bf9ee687f9b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_13@
hs_bindgen_2fdd1bf9ee687f9b :: IO (RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_2fdd1bf9ee687f9b =
  RIP.fromFFIType hs_bindgen_2fdd1bf9ee687f9b_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h 197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 :: IO (RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
fun_13 = hs_bindgen_2fdd1bf9ee687f9b

-- __unique:__ @test_arraysarray_Example_Safe_fun_14@
foreign import ccall safe "hs_bindgen_12a242894a3d45cb" hs_bindgen_12a242894a3d45cb_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_14@
hs_bindgen_12a242894a3d45cb :: IO (RIP.Ptr Matrix)
hs_bindgen_12a242894a3d45cb =
  RIP.fromFFIType hs_bindgen_12a242894a3d45cb_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h 200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 :: IO (RIP.Ptr Matrix)
fun_14 = hs_bindgen_12a242894a3d45cb

-- __unique:__ @test_arraysarray_Example_Safe_fun_15@
foreign import ccall safe "hs_bindgen_d8e176eb5efefa2c" hs_bindgen_d8e176eb5efefa2c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_15@
hs_bindgen_d8e176eb5efefa2c :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_d8e176eb5efefa2c =
  RIP.fromFFIType hs_bindgen_d8e176eb5efefa2c_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h 203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
fun_15 = hs_bindgen_d8e176eb5efefa2c

-- __unique:__ @test_arraysarray_Example_Safe_fun_16@
foreign import ccall safe "hs_bindgen_dcf234ca786626c7" hs_bindgen_dcf234ca786626c7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_fun_16@
hs_bindgen_dcf234ca786626c7 :: IO (RIP.Ptr Tripletlist)
hs_bindgen_dcf234ca786626c7 =
  RIP.fromFFIType hs_bindgen_dcf234ca786626c7_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h 206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 :: IO (RIP.Ptr Tripletlist)
fun_16 = hs_bindgen_dcf234ca786626c7

-- __unique:__ @test_arraysarray_Example_Safe_solve@
foreign import ccall safe "hs_bindgen_f80a5b6a2770c658" hs_bindgen_f80a5b6a2770c658_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Safe_solve@
hs_bindgen_f80a5b6a2770c658 :: IO (RIP.Ptr Sudoku)
hs_bindgen_f80a5b6a2770c658 =
  RIP.fromFFIType hs_bindgen_f80a5b6a2770c658_base

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h 209:10@

__exported by:__ @arrays\/array.h@
-}
solve :: IO (RIP.Ptr Sudoku)
solve = hs_bindgen_f80a5b6a2770c658
