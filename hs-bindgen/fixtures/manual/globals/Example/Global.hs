{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
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
  [ "#include <manual/globals.h>"
  , "/* test_manualglobals_Example_get_globalConfig_ptr */"
  , "__attribute__ ((const))"
  , "struct globalConfig *hs_bindgen_e5d5a0f4ce4b2b08 (void)"
  , "{"
  , "  return &globalConfig;"
  , "}"
  , "/* test_manualglobals_Example_get_nonExternGlobalInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_f034badbc299f27b (void)"
  , "{"
  , "  return &nonExternGlobalInt;"
  , "}"
  , "/* test_manualglobals_Example_get_globalConstant_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_c83c3e4e014bf39c (void)"
  , "{"
  , "  return &globalConstant;"
  , "}"
  , "/* test_manualglobals_Example_get_anotherGlobalConstant_ptr */"
  , "__attribute__ ((const))"
  , "ConstInt *hs_bindgen_2d6b9a52b97910a9 (void)"
  , "{"
  , "  return &anotherGlobalConstant;"
  , "}"
  , "/* test_manualglobals_Example_get_constArray1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_a804e6470cde45c2 (void))[4]"
  , "{"
  , "  return &constArray1;"
  , "}"
  , "/* test_manualglobals_Example_get_constArray2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_3cd4fc49a6bb5840 (void))[]"
  , "{"
  , "  return &constArray2;"
  , "}"
  , "/* test_manualglobals_Example_get_constTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple const *hs_bindgen_8c3024ef7f2b0594 (void)"
  , "{"
  , "  return &constTuple;"
  , "}"
  , "/* test_manualglobals_Example_get_nonConstTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple *hs_bindgen_e1200a75ed20a2d2 (void)"
  , "{"
  , "  return &nonConstTuple;"
  , "}"
  , "/* test_manualglobals_Example_get_Int_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_1d4f0442a6f47a9a (void)"
  , "{"
  , "  return &Int;"
  , "}"
  , "/* test_manualglobals_Example_get_constInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_188ef9ca039f4abc (void)"
  , "{"
  , "  return &constInt;"
  , "}"
  , "/* test_manualglobals_Example_get_ptrToInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_1fb9e392279def5a (void)"
  , "{"
  , "  return &ptrToInt;"
  , "}"
  , "/* test_manualglobals_Example_get_ptrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_4003d50d5f510514 (void)"
  , "{"
  , "  return &ptrToConstInt;"
  , "}"
  , "/* test_manualglobals_Example_get_constPtrToInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_c3df48685426f621 (void)"
  , "{"
  , "  return &constPtrToInt;"
  , "}"
  , "/* test_manualglobals_Example_get_constPtrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_7a4dc03eb19059c3 (void)"
  , "{"
  , "  return &constPtrToConstInt;"
  , "}"
  , "/* test_manualglobals_Example_get_a_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_06f3c9316bae45c0 (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* test_manualglobals_Example_get_a2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_4c8d80fc84c71290 (void)"
  , "{"
  , "  return &a2;"
  , "}"
  , "/* test_manualglobals_Example_get_b_ptr */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_e5c5169f8ce1e8e7 (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* test_manualglobals_Example_get_b2_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_43324e068ce594ea (void)"
  , "{"
  , "  return &b2;"
  , "}"
  , "/* test_manualglobals_Example_get_c_ptr */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_a4cd537fbce0dcf6 (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* test_manualglobals_Example_get_c2_ptr */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_1577ab8d87cfb457 (void)"
  , "{"
  , "  return &c2;"
  , "}"
  , "/* test_manualglobals_Example_get_d_ptr */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_6b8b44ba14dfea99 (void)"
  , "{"
  , "  return &d;"
  , "}"
  , "/* test_manualglobals_Example_get_d2_ptr */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_9e78609608fd60a1 (void)"
  , "{"
  , "  return &d2;"
  , "}"
  ]))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e5d5a0f4ce4b2b08" hs_bindgen_e5d5a0f4ce4b2b08_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_globalConfig_ptr@
hs_bindgen_e5d5a0f4ce4b2b08 ::
     IO (Ptr.Ptr GlobalConfig)
hs_bindgen_e5d5a0f4ce4b2b08 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e5d5a0f4ce4b2b08_base

{-# NOINLINE globalConfig_ptr #-}

{-| __C declaration:__ @globalConfig@

    __defined at:__ @manual\/globals.h:12:28@

    __exported by:__ @manual\/globals.h@
-}
globalConfig_ptr :: Ptr.Ptr GlobalConfig
globalConfig_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e5d5a0f4ce4b2b08

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_f034badbc299f27b" hs_bindgen_f034badbc299f27b_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_nonExternGlobalInt_ptr@
hs_bindgen_f034badbc299f27b ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_f034badbc299f27b =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_f034badbc299f27b_base

{-# NOINLINE nonExternGlobalInt_ptr #-}

{-| __C declaration:__ @nonExternGlobalInt@

    __defined at:__ @manual\/globals.h:18:5@

    __exported by:__ @manual\/globals.h@
-}
nonExternGlobalInt_ptr :: Ptr.Ptr FC.CInt
nonExternGlobalInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f034badbc299f27b

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c83c3e4e014bf39c" hs_bindgen_c83c3e4e014bf39c_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_globalConstant_ptr@
hs_bindgen_c83c3e4e014bf39c ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_c83c3e4e014bf39c =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c83c3e4e014bf39c_base

{-# NOINLINE globalConstant_ptr #-}

{-| __C declaration:__ @globalConstant@

    __defined at:__ @manual\/globals.h:39:18@

    __exported by:__ @manual\/globals.h@
-}
globalConstant_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
globalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c83c3e4e014bf39c

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr globalConstant_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_2d6b9a52b97910a9" hs_bindgen_2d6b9a52b97910a9_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_anotherGlobalConstant_ptr@
hs_bindgen_2d6b9a52b97910a9 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ConstInt)
hs_bindgen_2d6b9a52b97910a9 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_2d6b9a52b97910a9_base

{-# NOINLINE anotherGlobalConstant_ptr #-}

{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @manual\/globals.h:41:17@

    __exported by:__ @manual\/globals.h@
-}
anotherGlobalConstant_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_2d6b9a52b97910a9

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr anotherGlobalConstant_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a804e6470cde45c2" hs_bindgen_a804e6470cde45c2_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constArray1_ptr@
hs_bindgen_a804e6470cde45c2 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))
hs_bindgen_a804e6470cde45c2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a804e6470cde45c2_base

{-# NOINLINE constArray1_ptr #-}

{-| An array of known size of const ints

__C declaration:__ @constArray1@

__defined at:__ @manual\/globals.h:48:18@

__exported by:__ @manual\/globals.h@
-}
constArray1_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a804e6470cde45c2

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr constArray1_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_3cd4fc49a6bb5840" hs_bindgen_3cd4fc49a6bb5840_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constArray2_ptr@
hs_bindgen_3cd4fc49a6bb5840 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt))
hs_bindgen_3cd4fc49a6bb5840 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_3cd4fc49a6bb5840_base

{-# NOINLINE constArray2_ptr #-}

{-| An array of unknown size of const insts

__C declaration:__ @constArray2@

__defined at:__ @manual\/globals.h:50:18@

__exported by:__ @manual\/globals.h@
-}
constArray2_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt)
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3cd4fc49a6bb5840

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_8c3024ef7f2b0594" hs_bindgen_8c3024ef7f2b0594_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constTuple_ptr@
hs_bindgen_8c3024ef7f2b0594 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Tuple)
hs_bindgen_8c3024ef7f2b0594 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_8c3024ef7f2b0594_base

{-# NOINLINE constTuple_ptr #-}

{-| A constant tuple

__C declaration:__ @constTuple@

__defined at:__ @manual\/globals.h:54:27@

__exported by:__ @manual\/globals.h@
-}
constTuple_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8c3024ef7f2b0594

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr constTuple_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e1200a75ed20a2d2" hs_bindgen_e1200a75ed20a2d2_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_nonConstTuple_ptr@
hs_bindgen_e1200a75ed20a2d2 ::
     IO (Ptr.Ptr Tuple)
hs_bindgen_e1200a75ed20a2d2 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e1200a75ed20a2d2_base

{-# NOINLINE nonConstTuple_ptr #-}

{-| A non-constant tuple with a constant member

__C declaration:__ @nonConstTuple@

__defined at:__ @manual\/globals.h:56:21@

__exported by:__ @manual\/globals.h@
-}
nonConstTuple_ptr :: Ptr.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e1200a75ed20a2d2

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1d4f0442a6f47a9a" hs_bindgen_1d4f0442a6f47a9a_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_Int_ptr@
hs_bindgen_1d4f0442a6f47a9a ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_1d4f0442a6f47a9a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1d4f0442a6f47a9a_base

{-# NOINLINE int_ptr #-}

{-| An int

__C declaration:__ @Int@

__defined at:__ @manual\/globals.h:59:12@

__exported by:__ @manual\/globals.h@
-}
int_ptr :: Ptr.Ptr FC.CInt
int_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1d4f0442a6f47a9a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_188ef9ca039f4abc" hs_bindgen_188ef9ca039f4abc_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constInt_ptr@
hs_bindgen_188ef9ca039f4abc ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_188ef9ca039f4abc =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_188ef9ca039f4abc_base

{-# NOINLINE constInt_ptr #-}

{-| A const int

__C declaration:__ @constInt@

__defined at:__ @manual\/globals.h:61:18@

__exported by:__ @manual\/globals.h@
-}
constInt_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
constInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_188ef9ca039f4abc

{-# NOINLINE constInt #-}

constInt :: FC.CInt
constInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr constInt_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1fb9e392279def5a" hs_bindgen_1fb9e392279def5a_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_ptrToInt_ptr@
hs_bindgen_1fb9e392279def5a ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_1fb9e392279def5a =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1fb9e392279def5a_base

{-# NOINLINE ptrToInt_ptr #-}

{-| A pointer to int

__C declaration:__ @ptrToInt@

__defined at:__ @manual\/globals.h:63:14@

__exported by:__ @manual\/globals.h@
-}
ptrToInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
ptrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1fb9e392279def5a

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4003d50d5f510514" hs_bindgen_4003d50d5f510514_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_ptrToConstInt_ptr@
hs_bindgen_4003d50d5f510514 ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
hs_bindgen_4003d50d5f510514 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4003d50d5f510514_base

{-# NOINLINE ptrToConstInt_ptr #-}

{-| A pointer to const int

__C declaration:__ @ptrToConstInt@

__defined at:__ @manual\/globals.h:65:20@

__exported by:__ @manual\/globals.h@
-}
ptrToConstInt_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4003d50d5f510514

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_c3df48685426f621" hs_bindgen_c3df48685426f621_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constPtrToInt_ptr@
hs_bindgen_c3df48685426f621 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt))
hs_bindgen_c3df48685426f621 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_c3df48685426f621_base

{-# NOINLINE constPtrToInt_ptr #-}

{-| A const pointer to int

__C declaration:__ @constPtrToInt@

__defined at:__ @manual\/globals.h:67:20@

__exported by:__ @manual\/globals.h@
-}
constPtrToInt_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (Ptr.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c3df48685426f621

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: Ptr.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr constPtrToInt_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_7a4dc03eb19059c3" hs_bindgen_7a4dc03eb19059c3_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_constPtrToConstInt_ptr@
hs_bindgen_7a4dc03eb19059c3 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
hs_bindgen_7a4dc03eb19059c3 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_7a4dc03eb19059c3_base

{-# NOINLINE constPtrToConstInt_ptr #-}

{-| A const pointer to const int

__C declaration:__ @constPtrToConstInt@

__defined at:__ @manual\/globals.h:69:26@

__exported by:__ @manual\/globals.h@
-}
constPtrToConstInt_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7a4dc03eb19059c3

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr constPtrToConstInt_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_06f3c9316bae45c0" hs_bindgen_06f3c9316bae45c0_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_a_ptr@
hs_bindgen_06f3c9316bae45c0 ::
     IO (Ptr.Ptr FC.CInt)
hs_bindgen_06f3c9316bae45c0 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_06f3c9316bae45c0_base

{-# NOINLINE a_ptr #-}

{-| __C declaration:__ @a@

    __defined at:__ @manual\/globals.h:75:12@

    __exported by:__ @manual\/globals.h@
-}
a_ptr :: Ptr.Ptr FC.CInt
a_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_06f3c9316bae45c0

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_4c8d80fc84c71290" hs_bindgen_4c8d80fc84c71290_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_a2_ptr@
hs_bindgen_4c8d80fc84c71290 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
hs_bindgen_4c8d80fc84c71290 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_4c8d80fc84c71290_base

{-# NOINLINE a2_ptr #-}

{-| __C declaration:__ @a2@

    __defined at:__ @manual\/globals.h:76:18@

    __exported by:__ @manual\/globals.h@
-}
a2_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt
a2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4c8d80fc84c71290

{-# NOINLINE a2 #-}

a2 :: FC.CInt
a2 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr a2_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_e5c5169f8ce1e8e7" hs_bindgen_e5c5169f8ce1e8e7_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_b_ptr@
hs_bindgen_e5c5169f8ce1e8e7 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))
hs_bindgen_e5c5169f8ce1e8e7 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_e5c5169f8ce1e8e7_base

{-# NOINLINE b_ptr #-}

{-| __C declaration:__ @b@

    __defined at:__ @manual\/globals.h:78:14@

    __exported by:__ @manual\/globals.h@
-}
b_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
b_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e5c5169f8ce1e8e7

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_43324e068ce594ea" hs_bindgen_43324e068ce594ea_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_b2_ptr@
hs_bindgen_43324e068ce594ea ::
     IO (Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt))
hs_bindgen_43324e068ce594ea =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_43324e068ce594ea_base

{-# NOINLINE b2_ptr #-}

{-| __C declaration:__ @b2@

    __defined at:__ @manual\/globals.h:79:20@

    __exported by:__ @manual\/globals.h@
-}
b2_ptr :: Ptr.Ptr (HsBindgen.Runtime.ConstPtr.ConstPtr FC.CInt)
b2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_43324e068ce594ea

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_a4cd537fbce0dcf6" hs_bindgen_a4cd537fbce0dcf6_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_c_ptr@
hs_bindgen_a4cd537fbce0dcf6 ::
     IO (Ptr.Ptr Triplet)
hs_bindgen_a4cd537fbce0dcf6 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_a4cd537fbce0dcf6_base

{-# NOINLINE c_ptr #-}

{-| __C declaration:__ @c@

    __defined at:__ @manual\/globals.h:82:16@

    __exported by:__ @manual\/globals.h@
-}
c_ptr :: Ptr.Ptr Triplet
c_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a4cd537fbce0dcf6

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_1577ab8d87cfb457" hs_bindgen_1577ab8d87cfb457_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_c2_ptr@
hs_bindgen_1577ab8d87cfb457 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Triplet)
hs_bindgen_1577ab8d87cfb457 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_1577ab8d87cfb457_base

{-# NOINLINE c2_ptr #-}

{-| __C declaration:__ @c2@

    __defined at:__ @manual\/globals.h:83:22@

    __exported by:__ @manual\/globals.h@
-}
c2_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr Triplet
c2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_1577ab8d87cfb457

{-# NOINLINE c2 #-}

c2 :: Triplet
c2 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek (HsBindgen.Runtime.ConstPtr.unConstPtr c2_ptr))

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_6b8b44ba14dfea99" hs_bindgen_6b8b44ba14dfea99_base ::
     IO (Ptr.Ptr Void)

-- | __unique:__ @test_manualglobals_Example_get_d_ptr@
hs_bindgen_6b8b44ba14dfea99 ::
     IO (Ptr.Ptr List)
hs_bindgen_6b8b44ba14dfea99 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_6b8b44ba14dfea99_base

{-# NOINLINE d_ptr #-}

{-| __C declaration:__ @d@

    __defined at:__ @manual\/globals.h:86:13@

    __exported by:__ @manual\/globals.h@
-}
d_ptr :: Ptr.Ptr List
d_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_6b8b44ba14dfea99

{-| This is an internal function.
-}
foreign import ccall unsafe "hs_bindgen_9e78609608fd60a1" hs_bindgen_9e78609608fd60a1_base ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr Void)

-- | __unique:__ @test_manualglobals_Example_get_d2_ptr@
hs_bindgen_9e78609608fd60a1 ::
     IO (HsBindgen.Runtime.ConstPtr.ConstPtr List)
hs_bindgen_9e78609608fd60a1 =
  HsBindgen.Runtime.HasBaseForeignType.fromBaseForeignType hs_bindgen_9e78609608fd60a1_base

{-# NOINLINE d2_ptr #-}

{-| __C declaration:__ @d2@

    __defined at:__ @manual\/globals.h:87:19@

    __exported by:__ @manual\/globals.h@
-}
d2_ptr :: HsBindgen.Runtime.ConstPtr.ConstPtr List
d2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9e78609608fd60a1
