{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Unsafe where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <arrays/array.h>"
  , "signed int hs_bindgen_6d07a0b03f884547 ("
  , "  signed int arg1,"
  , "  signed int (*arg2)[3]"
  , ")"
  , "{"
  , "  return fun_1(arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_04318f98a3ab8d08 ("
  , "  triplet *arg1"
  , ")"
  , "{"
  , "  return fun_2(*arg1);"
  , "}"
  , "signed int hs_bindgen_2a7c5fa1040fa8db ("
  , "  signed int (*arg1)[]"
  , ")"
  , "{"
  , "  return fun_3(*arg1);"
  , "}"
  , "signed int hs_bindgen_810acc5cf8729d0e ("
  , "  list *arg1"
  , ")"
  , "{"
  , "  return fun_4(*arg1);"
  , "}"
  , "signed int hs_bindgen_83b71f7defb3b27a ("
  , "  signed int (*arg1)[4][3]"
  , ")"
  , "{"
  , "  return fun_5(*arg1);"
  , "}"
  , "signed int hs_bindgen_62b76af3dc65da3f ("
  , "  matrix *arg1"
  , ")"
  , "{"
  , "  return fun_6(*arg1);"
  , "}"
  , "signed int hs_bindgen_100aa7fb87a5ea74 ("
  , "  signed int (*arg1)[][3]"
  , ")"
  , "{"
  , "  return fun_7(*arg1);"
  , "}"
  , "signed int hs_bindgen_cd6646babeacd609 ("
  , "  tripletlist *arg1"
  , ")"
  , "{"
  , "  return fun_8(*arg1);"
  , "}"
  , "signed int hs_bindgen_560f1de9a83c3a6a ("
  , "  sudoku *arg1"
  , ")"
  , "{"
  , "  return isSolved(*arg1);"
  , "}"
  , "signed int hs_bindgen_ef3b85ae74bc06cf ("
  , "  signed int arg1,"
  , "  signed int (*arg2)[3],"
  , "  signed int const (*arg3)[3]"
  , ")"
  , "{"
  , "  return fun_1_const(arg1, *arg2, *arg3);"
  , "}"
  , "signed int hs_bindgen_1c913685e5e76952 ("
  , "  triplet *arg1,"
  , "  triplet const *arg2"
  , ")"
  , "{"
  , "  return fun_2_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_eb8daf22bd5c6f00 ("
  , "  signed int (*arg1)[],"
  , "  signed int const (*arg2)[]"
  , ")"
  , "{"
  , "  return fun_3_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_0b73e4c7695a3b2f ("
  , "  list *arg1,"
  , "  list const *arg2"
  , ")"
  , "{"
  , "  return fun_4_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_374feb8086895fe3 ("
  , "  signed int (*arg1)[4][3],"
  , "  signed int const (*arg2)[4][3]"
  , ")"
  , "{"
  , "  return fun_5_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_2d1320b468c36708 ("
  , "  matrix *arg1,"
  , "  matrix const *arg2"
  , ")"
  , "{"
  , "  return fun_6_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_f67f5fe5bfb57aa1 ("
  , "  signed int (*arg1)[][3],"
  , "  signed int const (*arg2)[][3]"
  , ")"
  , "{"
  , "  return fun_7_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_2c9356851d76320e ("
  , "  tripletlist *arg1,"
  , "  tripletlist const *arg2"
  , ")"
  , "{"
  , "  return fun_8_const(*arg1, *arg2);"
  , "}"
  , "signed int hs_bindgen_39b08b64fed0c5b8 ("
  , "  sudoku *arg1,"
  , "  sudoku const *arg2"
  , ")"
  , "{"
  , "  return isSolved_const(*arg1, *arg2);"
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
foreign import ccall unsafe "hs_bindgen_6d07a0b03f884547" hs_bindgen_6d07a0b03f884547_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_1@
hs_bindgen_6d07a0b03f884547 ::
     RIP.CInt
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_6d07a0b03f884547 =
  RIP.fromFFIType hs_bindgen_6d07a0b03f884547_base

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
fun_1 = hs_bindgen_6d07a0b03f884547

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2@
foreign import ccall unsafe "hs_bindgen_04318f98a3ab8d08" hs_bindgen_04318f98a3ab8d08_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2@
hs_bindgen_04318f98a3ab8d08 ::
     RIP.Ptr Triplet
  -> IO RIP.CInt
hs_bindgen_04318f98a3ab8d08 =
  RIP.fromFFIType hs_bindgen_04318f98a3ab8d08_base

{-| Array of known size, typedef

__C declaration:__ @fun_2@

__defined at:__ @arrays\/array.h 121:5@

__exported by:__ @arrays\/array.h@
-}
fun_2 ::
     RIP.Ptr Triplet
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_2 = hs_bindgen_04318f98a3ab8d08

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3@
foreign import ccall unsafe "hs_bindgen_2a7c5fa1040fa8db" hs_bindgen_2a7c5fa1040fa8db_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3@
hs_bindgen_2a7c5fa1040fa8db ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_2a7c5fa1040fa8db =
  RIP.fromFFIType hs_bindgen_2a7c5fa1040fa8db_base

{-| Array of unknown size

__C declaration:__ @fun_3@

__defined at:__ @arrays\/array.h 124:5@

__exported by:__ @arrays\/array.h@
-}
fun_3 ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_3 = hs_bindgen_2a7c5fa1040fa8db

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4@
foreign import ccall unsafe "hs_bindgen_810acc5cf8729d0e" hs_bindgen_810acc5cf8729d0e_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4@
hs_bindgen_810acc5cf8729d0e ::
     RIP.Ptr List
  -> IO RIP.CInt
hs_bindgen_810acc5cf8729d0e =
  RIP.fromFFIType hs_bindgen_810acc5cf8729d0e_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_4@

__defined at:__ @arrays\/array.h 127:5@

__exported by:__ @arrays\/array.h@
-}
fun_4 ::
     RIP.Ptr List
     -- ^ __C declaration:__ @xs@
  -> IO RIP.CInt
fun_4 = hs_bindgen_810acc5cf8729d0e

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5@
foreign import ccall unsafe "hs_bindgen_83b71f7defb3b27a" hs_bindgen_83b71f7defb3b27a_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5@
hs_bindgen_83b71f7defb3b27a ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_83b71f7defb3b27a =
  RIP.fromFFIType hs_bindgen_83b71f7defb3b27a_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_5@

__defined at:__ @arrays\/array.h 130:5@

__exported by:__ @arrays\/array.h@
-}
fun_5 ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_5 = hs_bindgen_83b71f7defb3b27a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6@
foreign import ccall unsafe "hs_bindgen_62b76af3dc65da3f" hs_bindgen_62b76af3dc65da3f_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6@
hs_bindgen_62b76af3dc65da3f ::
     RIP.Ptr Matrix
  -> IO RIP.CInt
hs_bindgen_62b76af3dc65da3f =
  RIP.fromFFIType hs_bindgen_62b76af3dc65da3f_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_6@

__defined at:__ @arrays\/array.h 133:5@

__exported by:__ @arrays\/array.h@
-}
fun_6 ::
     RIP.Ptr Matrix
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_6 = hs_bindgen_62b76af3dc65da3f

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7@
foreign import ccall unsafe "hs_bindgen_100aa7fb87a5ea74" hs_bindgen_100aa7fb87a5ea74_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7@
hs_bindgen_100aa7fb87a5ea74 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_100aa7fb87a5ea74 =
  RIP.fromFFIType hs_bindgen_100aa7fb87a5ea74_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_7@

__defined at:__ @arrays\/array.h 136:5@

__exported by:__ @arrays\/array.h@
-}
fun_7 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_7 = hs_bindgen_100aa7fb87a5ea74

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8@
foreign import ccall unsafe "hs_bindgen_cd6646babeacd609" hs_bindgen_cd6646babeacd609_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8@
hs_bindgen_cd6646babeacd609 ::
     RIP.Ptr Tripletlist
  -> IO RIP.CInt
hs_bindgen_cd6646babeacd609 =
  RIP.fromFFIType hs_bindgen_cd6646babeacd609_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_8@

__defined at:__ @arrays\/array.h 139:5@

__exported by:__ @arrays\/array.h@
-}
fun_8 ::
     RIP.Ptr Tripletlist
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
fun_8 = hs_bindgen_cd6646babeacd609

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved@
foreign import ccall unsafe "hs_bindgen_560f1de9a83c3a6a" hs_bindgen_560f1de9a83c3a6a_base ::
     RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved@
hs_bindgen_560f1de9a83c3a6a ::
     RIP.Ptr Sudoku
  -> IO RIP.CInt
hs_bindgen_560f1de9a83c3a6a =
  RIP.fromFFIType hs_bindgen_560f1de9a83c3a6a_base

{-| Typedef-in-typedef

__C declaration:__ @isSolved@

__defined at:__ @arrays\/array.h 142:5@

__exported by:__ @arrays\/array.h@
-}
isSolved ::
     RIP.Ptr Sudoku
     -- ^ __C declaration:__ @xss@
  -> IO RIP.CInt
isSolved = hs_bindgen_560f1de9a83c3a6a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_1_const@
foreign import ccall unsafe "hs_bindgen_ef3b85ae74bc06cf" hs_bindgen_ef3b85ae74bc06cf_base ::
     RIP.Int32
  -> RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_1_const@
hs_bindgen_ef3b85ae74bc06cf ::
     RIP.CInt
  -> RIP.Ptr ((CA.ConstantArray 3) RIP.CInt)
  -> PtrConst.PtrConst ((CA.ConstantArray 3) RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_ef3b85ae74bc06cf =
  RIP.fromFFIType hs_bindgen_ef3b85ae74bc06cf_base

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
fun_1_const = hs_bindgen_ef3b85ae74bc06cf

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2_const@
foreign import ccall unsafe "hs_bindgen_1c913685e5e76952" hs_bindgen_1c913685e5e76952_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_2_const@
hs_bindgen_1c913685e5e76952 ::
     RIP.Ptr Triplet
  -> PtrConst.PtrConst Triplet
  -> IO RIP.CInt
hs_bindgen_1c913685e5e76952 =
  RIP.fromFFIType hs_bindgen_1c913685e5e76952_base

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
fun_2_const = hs_bindgen_1c913685e5e76952

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3_const@
foreign import ccall unsafe "hs_bindgen_eb8daf22bd5c6f00" hs_bindgen_eb8daf22bd5c6f00_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_3_const@
hs_bindgen_eb8daf22bd5c6f00 ::
     RIP.Ptr (IA.IncompleteArray RIP.CInt)
  -> PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
  -> IO RIP.CInt
hs_bindgen_eb8daf22bd5c6f00 =
  RIP.fromFFIType hs_bindgen_eb8daf22bd5c6f00_base

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
fun_3_const = hs_bindgen_eb8daf22bd5c6f00

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4_const@
foreign import ccall unsafe "hs_bindgen_0b73e4c7695a3b2f" hs_bindgen_0b73e4c7695a3b2f_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_4_const@
hs_bindgen_0b73e4c7695a3b2f ::
     RIP.Ptr List
  -> PtrConst.PtrConst List
  -> IO RIP.CInt
hs_bindgen_0b73e4c7695a3b2f =
  RIP.fromFFIType hs_bindgen_0b73e4c7695a3b2f_base

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
fun_4_const = hs_bindgen_0b73e4c7695a3b2f

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5_const@
foreign import ccall unsafe "hs_bindgen_374feb8086895fe3" hs_bindgen_374feb8086895fe3_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_5_const@
hs_bindgen_374feb8086895fe3 ::
     RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> PtrConst.PtrConst ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_374feb8086895fe3 =
  RIP.fromFFIType hs_bindgen_374feb8086895fe3_base

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
fun_5_const = hs_bindgen_374feb8086895fe3

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6_const@
foreign import ccall unsafe "hs_bindgen_2d1320b468c36708" hs_bindgen_2d1320b468c36708_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_6_const@
hs_bindgen_2d1320b468c36708 ::
     RIP.Ptr Matrix
  -> PtrConst.PtrConst Matrix
  -> IO RIP.CInt
hs_bindgen_2d1320b468c36708 =
  RIP.fromFFIType hs_bindgen_2d1320b468c36708_base

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
fun_6_const = hs_bindgen_2d1320b468c36708

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7_const@
foreign import ccall unsafe "hs_bindgen_f67f5fe5bfb57aa1" hs_bindgen_f67f5fe5bfb57aa1_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_7_const@
hs_bindgen_f67f5fe5bfb57aa1 ::
     RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> PtrConst.PtrConst (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt))
  -> IO RIP.CInt
hs_bindgen_f67f5fe5bfb57aa1 =
  RIP.fromFFIType hs_bindgen_f67f5fe5bfb57aa1_base

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
fun_7_const = hs_bindgen_f67f5fe5bfb57aa1

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8_const@
foreign import ccall unsafe "hs_bindgen_2c9356851d76320e" hs_bindgen_2c9356851d76320e_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_8_const@
hs_bindgen_2c9356851d76320e ::
     RIP.Ptr Tripletlist
  -> PtrConst.PtrConst Tripletlist
  -> IO RIP.CInt
hs_bindgen_2c9356851d76320e =
  RIP.fromFFIType hs_bindgen_2c9356851d76320e_base

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
fun_8_const = hs_bindgen_2c9356851d76320e

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved_const@
foreign import ccall unsafe "hs_bindgen_39b08b64fed0c5b8" hs_bindgen_39b08b64fed0c5b8_base ::
     RIP.Ptr RIP.Void
  -> RIP.Ptr RIP.Void
  -> IO RIP.Int32

-- __unique:__ @test_arraysarray_Example_Unsafe_isSolved_const@
hs_bindgen_39b08b64fed0c5b8 ::
     RIP.Ptr Sudoku
  -> PtrConst.PtrConst Sudoku
  -> IO RIP.CInt
hs_bindgen_39b08b64fed0c5b8 =
  RIP.fromFFIType hs_bindgen_39b08b64fed0c5b8_base

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
isSolved_const = hs_bindgen_39b08b64fed0c5b8

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_9@
foreign import ccall unsafe "hs_bindgen_ab431ebc0519545a" hs_bindgen_ab431ebc0519545a_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_9@
hs_bindgen_ab431ebc0519545a :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
hs_bindgen_ab431ebc0519545a =
  RIP.fromFFIType hs_bindgen_ab431ebc0519545a_base

{-| Array of known size

__C declaration:__ @fun_9@

__defined at:__ @arrays\/array.h 185:7@

__exported by:__ @arrays\/array.h@
-}
fun_9 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CInt))
fun_9 = hs_bindgen_ab431ebc0519545a

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_10@
foreign import ccall unsafe "hs_bindgen_c9ff623e6f48d3bc" hs_bindgen_c9ff623e6f48d3bc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_10@
hs_bindgen_c9ff623e6f48d3bc :: IO (RIP.Ptr Triplet)
hs_bindgen_c9ff623e6f48d3bc =
  RIP.fromFFIType hs_bindgen_c9ff623e6f48d3bc_base

{-| Array of known size, typedef

__C declaration:__ @fun_10@

__defined at:__ @arrays\/array.h 188:10@

__exported by:__ @arrays\/array.h@
-}
fun_10 :: IO (RIP.Ptr Triplet)
fun_10 = hs_bindgen_c9ff623e6f48d3bc

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_11@
foreign import ccall unsafe "hs_bindgen_e714f0b7c764ba17" hs_bindgen_e714f0b7c764ba17_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_11@
hs_bindgen_e714f0b7c764ba17 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
hs_bindgen_e714f0b7c764ba17 =
  RIP.fromFFIType hs_bindgen_e714f0b7c764ba17_base

{-| Array of unknown size

__C declaration:__ @fun_11@

__defined at:__ @arrays\/array.h 191:7@

__exported by:__ @arrays\/array.h@
-}
fun_11 :: IO (RIP.Ptr (IA.IncompleteArray RIP.CInt))
fun_11 = hs_bindgen_e714f0b7c764ba17

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_12@
foreign import ccall unsafe "hs_bindgen_cc23741700ba18f7" hs_bindgen_cc23741700ba18f7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_12@
hs_bindgen_cc23741700ba18f7 :: IO (RIP.Ptr List)
hs_bindgen_cc23741700ba18f7 =
  RIP.fromFFIType hs_bindgen_cc23741700ba18f7_base

{-| Array of unknown size, typedef

__C declaration:__ @fun_12@

__defined at:__ @arrays\/array.h 194:7@

__exported by:__ @arrays\/array.h@
-}
fun_12 :: IO (RIP.Ptr List)
fun_12 = hs_bindgen_cc23741700ba18f7

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_13@
foreign import ccall unsafe "hs_bindgen_eb3a1364003829ac" hs_bindgen_eb3a1364003829ac_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_13@
hs_bindgen_eb3a1364003829ac :: IO (RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_eb3a1364003829ac =
  RIP.fromFFIType hs_bindgen_eb3a1364003829ac_base

{-| Multi-dimensional array of known size

__C declaration:__ @fun_13@

__defined at:__ @arrays\/array.h 197:7@

__exported by:__ @arrays\/array.h@
-}
fun_13 :: IO (RIP.Ptr ((CA.ConstantArray 4) ((CA.ConstantArray 3) RIP.CInt)))
fun_13 = hs_bindgen_eb3a1364003829ac

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_14@
foreign import ccall unsafe "hs_bindgen_0f49ffbe2c13ab46" hs_bindgen_0f49ffbe2c13ab46_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_14@
hs_bindgen_0f49ffbe2c13ab46 :: IO (RIP.Ptr Matrix)
hs_bindgen_0f49ffbe2c13ab46 =
  RIP.fromFFIType hs_bindgen_0f49ffbe2c13ab46_base

{-| Multi-dimensional array of known size, typedef

__C declaration:__ @fun_14@

__defined at:__ @arrays\/array.h 200:9@

__exported by:__ @arrays\/array.h@
-}
fun_14 :: IO (RIP.Ptr Matrix)
fun_14 = hs_bindgen_0f49ffbe2c13ab46

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_15@
foreign import ccall unsafe "hs_bindgen_59de769fbba4ed72" hs_bindgen_59de769fbba4ed72_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_15@
hs_bindgen_59de769fbba4ed72 :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
hs_bindgen_59de769fbba4ed72 =
  RIP.fromFFIType hs_bindgen_59de769fbba4ed72_base

{-| Multi-dimensional array of unknown size

__C declaration:__ @fun_15@

__defined at:__ @arrays\/array.h 203:7@

__exported by:__ @arrays\/array.h@
-}
fun_15 :: IO (RIP.Ptr (IA.IncompleteArray ((CA.ConstantArray 3) RIP.CInt)))
fun_15 = hs_bindgen_59de769fbba4ed72

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_16@
foreign import ccall unsafe "hs_bindgen_1d6ecccfa4ee16ff" hs_bindgen_1d6ecccfa4ee16ff_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_fun_16@
hs_bindgen_1d6ecccfa4ee16ff :: IO (RIP.Ptr Tripletlist)
hs_bindgen_1d6ecccfa4ee16ff =
  RIP.fromFFIType hs_bindgen_1d6ecccfa4ee16ff_base

{-| Multi-dimensional array of unknown size, typedef

__C declaration:__ @fun_16@

__defined at:__ @arrays\/array.h 206:14@

__exported by:__ @arrays\/array.h@
-}
fun_16 :: IO (RIP.Ptr Tripletlist)
fun_16 = hs_bindgen_1d6ecccfa4ee16ff

-- __unique:__ @test_arraysarray_Example_Unsafe_solve@
foreign import ccall unsafe "hs_bindgen_6165085eab7d2806" hs_bindgen_6165085eab7d2806_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_arraysarray_Example_Unsafe_solve@
hs_bindgen_6165085eab7d2806 :: IO (RIP.Ptr Sudoku)
hs_bindgen_6165085eab7d2806 =
  RIP.fromFFIType hs_bindgen_6165085eab7d2806_base

{-| Typedef-in-typedef

__C declaration:__ @solve@

__defined at:__ @arrays\/array.h 209:10@

__exported by:__ @arrays\/array.h@
-}
solve :: IO (RIP.Ptr Sudoku)
solve = hs_bindgen_6165085eab7d2806
