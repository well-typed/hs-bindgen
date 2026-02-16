{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.IncompleteArray as IA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
  [ "#include <manual/globals.h>"
  , "/* test_manualglobals_Example_get_globalConfig */"
  , "__attribute__ ((const))"
  , "struct globalConfig *hs_bindgen_1b18b112a828e595 (void)"
  , "{"
  , "  return &globalConfig;"
  , "}"
  , "/* test_manualglobals_Example_get_nonExternGlobalInt */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_3cc5675744591425 (void)"
  , "{"
  , "  return &nonExternGlobalInt;"
  , "}"
  , "/* test_manualglobals_Example_get_globalConstant */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_d1d21f66d76e4647 (void)"
  , "{"
  , "  return &globalConstant;"
  , "}"
  , "/* test_manualglobals_Example_get_anotherGlobalConstant */"
  , "__attribute__ ((const))"
  , "ConstInt *hs_bindgen_7b35192d93d6ddac (void)"
  , "{"
  , "  return &anotherGlobalConstant;"
  , "}"
  , "/* test_manualglobals_Example_get_constArray1 */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_8850207312ee9ee9 (void))[4]"
  , "{"
  , "  return &constArray1;"
  , "}"
  , "/* test_manualglobals_Example_get_constArray2 */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_c6ce2097d2a21fc1 (void))[]"
  , "{"
  , "  return &constArray2;"
  , "}"
  , "/* test_manualglobals_Example_get_constTuple */"
  , "__attribute__ ((const))"
  , "struct tuple const *hs_bindgen_ab75c76aaa5e35e5 (void)"
  , "{"
  , "  return &constTuple;"
  , "}"
  , "/* test_manualglobals_Example_get_nonConstTuple */"
  , "__attribute__ ((const))"
  , "struct tuple *hs_bindgen_a2454ae941c93b30 (void)"
  , "{"
  , "  return &nonConstTuple;"
  , "}"
  , "/* test_manualglobals_Example_get_Int */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_87910ad4d5b6d50b (void)"
  , "{"
  , "  return &Int;"
  , "}"
  , "/* test_manualglobals_Example_get_constInt */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_8f863fd75ee6a8b5 (void)"
  , "{"
  , "  return &constInt;"
  , "}"
  , "/* test_manualglobals_Example_get_ptrToInt */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_1a7a96e5c811e2d1 (void)"
  , "{"
  , "  return &ptrToInt;"
  , "}"
  , "/* test_manualglobals_Example_get_ptrToConstInt */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_65c37466b4a14692 (void)"
  , "{"
  , "  return &ptrToConstInt;"
  , "}"
  , "/* test_manualglobals_Example_get_constPtrToInt */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_78280019ff2647f8 (void)"
  , "{"
  , "  return &constPtrToInt;"
  , "}"
  , "/* test_manualglobals_Example_get_constPtrToConstInt */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_b7f5f49d98fc03a2 (void)"
  , "{"
  , "  return &constPtrToConstInt;"
  , "}"
  , "/* test_manualglobals_Example_get_a */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_6a66996f25c871b0 (void)"
  , "{"
  , "  return &a;"
  , "}"
  , "/* test_manualglobals_Example_get_a2 */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_69eaf4375e9019f6 (void)"
  , "{"
  , "  return &a2;"
  , "}"
  , "/* test_manualglobals_Example_get_b */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_86029851b6c19d0e (void)"
  , "{"
  , "  return &b;"
  , "}"
  , "/* test_manualglobals_Example_get_b2 */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_54ab82d9854a31c0 (void)"
  , "{"
  , "  return &b2;"
  , "}"
  , "/* test_manualglobals_Example_get_c */"
  , "__attribute__ ((const))"
  , "triplet *hs_bindgen_11d5e5406d066dc2 (void)"
  , "{"
  , "  return &c;"
  , "}"
  , "/* test_manualglobals_Example_get_c2 */"
  , "__attribute__ ((const))"
  , "triplet const *hs_bindgen_05a85a68bb53daff (void)"
  , "{"
  , "  return &c2;"
  , "}"
  , "/* test_manualglobals_Example_get_d */"
  , "__attribute__ ((const))"
  , "list *hs_bindgen_8fe22549222d7024 (void)"
  , "{"
  , "  return &d;"
  , "}"
  , "/* test_manualglobals_Example_get_d2 */"
  , "__attribute__ ((const))"
  , "list const *hs_bindgen_3b641c8e9aefc9e3 (void)"
  , "{"
  , "  return &d2;"
  , "}"
  ]))

-- __unique:__ @test_manualglobals_Example_get_globalConfig@
foreign import ccall unsafe "hs_bindgen_1b18b112a828e595" hs_bindgen_1b18b112a828e595_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_globalConfig@
hs_bindgen_1b18b112a828e595 :: IO (RIP.Ptr GlobalConfig)
hs_bindgen_1b18b112a828e595 =
  RIP.fromFFIType hs_bindgen_1b18b112a828e595_base

{-# NOINLINE globalConfig #-}
{-| __C declaration:__ @globalConfig@

    __defined at:__ @manual\/globals.h 12:28@

    __exported by:__ @manual\/globals.h@
-}
globalConfig :: RIP.Ptr GlobalConfig
globalConfig =
  RIP.unsafePerformIO hs_bindgen_1b18b112a828e595

-- __unique:__ @test_manualglobals_Example_get_nonExternGlobalInt@
foreign import ccall unsafe "hs_bindgen_3cc5675744591425" hs_bindgen_3cc5675744591425_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_nonExternGlobalInt@
hs_bindgen_3cc5675744591425 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_3cc5675744591425 =
  RIP.fromFFIType hs_bindgen_3cc5675744591425_base

{-# NOINLINE nonExternGlobalInt #-}
{-| __C declaration:__ @nonExternGlobalInt@

    __defined at:__ @manual\/globals.h 18:5@

    __exported by:__ @manual\/globals.h@
-}
nonExternGlobalInt :: RIP.Ptr RIP.CInt
nonExternGlobalInt =
  RIP.unsafePerformIO hs_bindgen_3cc5675744591425

-- __unique:__ @test_manualglobals_Example_get_globalConstant@
foreign import ccall unsafe "hs_bindgen_d1d21f66d76e4647" hs_bindgen_d1d21f66d76e4647_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_globalConstant@
hs_bindgen_d1d21f66d76e4647 :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_d1d21f66d76e4647 =
  RIP.fromFFIType hs_bindgen_d1d21f66d76e4647_base

{-# NOINLINE hs_bindgen_ec20fad59e967cf0 #-}
{-| __C declaration:__ @globalConstant@

    __defined at:__ @manual\/globals.h 39:18@

    __exported by:__ @manual\/globals.h@

    __unique:__ @test_manualglobals_Example_globalConstant@
-}
hs_bindgen_ec20fad59e967cf0 :: PtrConst.PtrConst RIP.CInt
hs_bindgen_ec20fad59e967cf0 =
  RIP.unsafePerformIO hs_bindgen_d1d21f66d76e4647

{-# NOINLINE globalConstant #-}
globalConstant :: RIP.CInt
globalConstant =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_ec20fad59e967cf0)

-- __unique:__ @test_manualglobals_Example_get_anotherGlobalConstant@
foreign import ccall unsafe "hs_bindgen_7b35192d93d6ddac" hs_bindgen_7b35192d93d6ddac_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_anotherGlobalConstant@
hs_bindgen_7b35192d93d6ddac :: IO (PtrConst.PtrConst ConstInt)
hs_bindgen_7b35192d93d6ddac =
  RIP.fromFFIType hs_bindgen_7b35192d93d6ddac_base

{-# NOINLINE hs_bindgen_c8344b69b41647d0 #-}
{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @manual\/globals.h 41:17@

    __exported by:__ @manual\/globals.h@

    __unique:__ @test_manualglobals_Example_anotherGlobalConstant@
-}
hs_bindgen_c8344b69b41647d0 :: PtrConst.PtrConst ConstInt
hs_bindgen_c8344b69b41647d0 =
  RIP.unsafePerformIO hs_bindgen_7b35192d93d6ddac

{-# NOINLINE anotherGlobalConstant #-}
anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_c8344b69b41647d0)

-- __unique:__ @test_manualglobals_Example_get_constArray1@
foreign import ccall unsafe "hs_bindgen_8850207312ee9ee9" hs_bindgen_8850207312ee9ee9_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constArray1@
hs_bindgen_8850207312ee9ee9 :: IO (PtrConst.PtrConst ((CA.ConstantArray 4) RIP.CInt))
hs_bindgen_8850207312ee9ee9 =
  RIP.fromFFIType hs_bindgen_8850207312ee9ee9_base

{-# NOINLINE hs_bindgen_ab47d9c9aff882f7 #-}
{-| An array of known size of const ints

__C declaration:__ @constArray1@

__defined at:__ @manual\/globals.h 48:18@

__exported by:__ @manual\/globals.h@

__unique:__ @test_manualglobals_Example_constArray1@
-}
hs_bindgen_ab47d9c9aff882f7 :: PtrConst.PtrConst ((CA.ConstantArray 4) RIP.CInt)
hs_bindgen_ab47d9c9aff882f7 =
  RIP.unsafePerformIO hs_bindgen_8850207312ee9ee9

{-# NOINLINE constArray1 #-}
constArray1 :: (CA.ConstantArray 4) RIP.CInt
constArray1 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_ab47d9c9aff882f7)

-- __unique:__ @test_manualglobals_Example_get_constArray2@
foreign import ccall unsafe "hs_bindgen_c6ce2097d2a21fc1" hs_bindgen_c6ce2097d2a21fc1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constArray2@
hs_bindgen_c6ce2097d2a21fc1 :: IO (PtrConst.PtrConst (IA.IncompleteArray RIP.CInt))
hs_bindgen_c6ce2097d2a21fc1 =
  RIP.fromFFIType hs_bindgen_c6ce2097d2a21fc1_base

{-# NOINLINE constArray2 #-}
{-| An array of unknown size of const insts

__C declaration:__ @constArray2@

__defined at:__ @manual\/globals.h 50:18@

__exported by:__ @manual\/globals.h@
-}
constArray2 :: PtrConst.PtrConst (IA.IncompleteArray RIP.CInt)
constArray2 =
  RIP.unsafePerformIO hs_bindgen_c6ce2097d2a21fc1

-- __unique:__ @test_manualglobals_Example_get_constTuple@
foreign import ccall unsafe "hs_bindgen_ab75c76aaa5e35e5" hs_bindgen_ab75c76aaa5e35e5_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constTuple@
hs_bindgen_ab75c76aaa5e35e5 :: IO (PtrConst.PtrConst Tuple)
hs_bindgen_ab75c76aaa5e35e5 =
  RIP.fromFFIType hs_bindgen_ab75c76aaa5e35e5_base

{-# NOINLINE hs_bindgen_957dfe4346c1834f #-}
{-| A constant tuple

__C declaration:__ @constTuple@

__defined at:__ @manual\/globals.h 54:27@

__exported by:__ @manual\/globals.h@

__unique:__ @test_manualglobals_Example_constTuple@
-}
hs_bindgen_957dfe4346c1834f :: PtrConst.PtrConst Tuple
hs_bindgen_957dfe4346c1834f =
  RIP.unsafePerformIO hs_bindgen_ab75c76aaa5e35e5

{-# NOINLINE constTuple #-}
constTuple :: Tuple
constTuple =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_957dfe4346c1834f)

-- __unique:__ @test_manualglobals_Example_get_nonConstTuple@
foreign import ccall unsafe "hs_bindgen_a2454ae941c93b30" hs_bindgen_a2454ae941c93b30_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_nonConstTuple@
hs_bindgen_a2454ae941c93b30 :: IO (RIP.Ptr Tuple)
hs_bindgen_a2454ae941c93b30 =
  RIP.fromFFIType hs_bindgen_a2454ae941c93b30_base

{-# NOINLINE nonConstTuple #-}
{-| A non-constant tuple with a constant member

__C declaration:__ @nonConstTuple@

__defined at:__ @manual\/globals.h 56:21@

__exported by:__ @manual\/globals.h@
-}
nonConstTuple :: RIP.Ptr Tuple
nonConstTuple =
  RIP.unsafePerformIO hs_bindgen_a2454ae941c93b30

-- __unique:__ @test_manualglobals_Example_get_Int@
foreign import ccall unsafe "hs_bindgen_87910ad4d5b6d50b" hs_bindgen_87910ad4d5b6d50b_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_Int@
hs_bindgen_87910ad4d5b6d50b :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_87910ad4d5b6d50b =
  RIP.fromFFIType hs_bindgen_87910ad4d5b6d50b_base

{-# NOINLINE int #-}
{-| An int

__C declaration:__ @Int@

__defined at:__ @manual\/globals.h 59:12@

__exported by:__ @manual\/globals.h@
-}
int :: RIP.Ptr RIP.CInt
int = RIP.unsafePerformIO hs_bindgen_87910ad4d5b6d50b

-- __unique:__ @test_manualglobals_Example_get_constInt@
foreign import ccall unsafe "hs_bindgen_8f863fd75ee6a8b5" hs_bindgen_8f863fd75ee6a8b5_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constInt@
hs_bindgen_8f863fd75ee6a8b5 :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_8f863fd75ee6a8b5 =
  RIP.fromFFIType hs_bindgen_8f863fd75ee6a8b5_base

{-# NOINLINE hs_bindgen_e999b97ca3936542 #-}
{-| A const int

__C declaration:__ @constInt@

__defined at:__ @manual\/globals.h 61:18@

__exported by:__ @manual\/globals.h@

__unique:__ @test_manualglobals_Example_constInt@
-}
hs_bindgen_e999b97ca3936542 :: PtrConst.PtrConst RIP.CInt
hs_bindgen_e999b97ca3936542 =
  RIP.unsafePerformIO hs_bindgen_8f863fd75ee6a8b5

{-# NOINLINE constInt #-}
constInt :: RIP.CInt
constInt =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_e999b97ca3936542)

-- __unique:__ @test_manualglobals_Example_get_ptrToInt@
foreign import ccall unsafe "hs_bindgen_1a7a96e5c811e2d1" hs_bindgen_1a7a96e5c811e2d1_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_ptrToInt@
hs_bindgen_1a7a96e5c811e2d1 :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_1a7a96e5c811e2d1 =
  RIP.fromFFIType hs_bindgen_1a7a96e5c811e2d1_base

{-# NOINLINE ptrToInt #-}
{-| A pointer to int

__C declaration:__ @ptrToInt@

__defined at:__ @manual\/globals.h 63:14@

__exported by:__ @manual\/globals.h@
-}
ptrToInt :: RIP.Ptr (RIP.Ptr RIP.CInt)
ptrToInt =
  RIP.unsafePerformIO hs_bindgen_1a7a96e5c811e2d1

-- __unique:__ @test_manualglobals_Example_get_ptrToConstInt@
foreign import ccall unsafe "hs_bindgen_65c37466b4a14692" hs_bindgen_65c37466b4a14692_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_ptrToConstInt@
hs_bindgen_65c37466b4a14692 :: IO (RIP.Ptr (PtrConst.PtrConst RIP.CInt))
hs_bindgen_65c37466b4a14692 =
  RIP.fromFFIType hs_bindgen_65c37466b4a14692_base

{-# NOINLINE ptrToConstInt #-}
{-| A pointer to const int

__C declaration:__ @ptrToConstInt@

__defined at:__ @manual\/globals.h 65:20@

__exported by:__ @manual\/globals.h@
-}
ptrToConstInt :: RIP.Ptr (PtrConst.PtrConst RIP.CInt)
ptrToConstInt =
  RIP.unsafePerformIO hs_bindgen_65c37466b4a14692

-- __unique:__ @test_manualglobals_Example_get_constPtrToInt@
foreign import ccall unsafe "hs_bindgen_78280019ff2647f8" hs_bindgen_78280019ff2647f8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constPtrToInt@
hs_bindgen_78280019ff2647f8 :: IO (PtrConst.PtrConst (RIP.Ptr RIP.CInt))
hs_bindgen_78280019ff2647f8 =
  RIP.fromFFIType hs_bindgen_78280019ff2647f8_base

{-# NOINLINE hs_bindgen_7a3b424845cc1ed9 #-}
{-| A const pointer to int

__C declaration:__ @constPtrToInt@

__defined at:__ @manual\/globals.h 67:20@

__exported by:__ @manual\/globals.h@

__unique:__ @test_manualglobals_Example_constPtrToInt@
-}
hs_bindgen_7a3b424845cc1ed9 :: PtrConst.PtrConst (RIP.Ptr RIP.CInt)
hs_bindgen_7a3b424845cc1ed9 =
  RIP.unsafePerformIO hs_bindgen_78280019ff2647f8

{-# NOINLINE constPtrToInt #-}
constPtrToInt :: RIP.Ptr RIP.CInt
constPtrToInt =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_7a3b424845cc1ed9)

-- __unique:__ @test_manualglobals_Example_get_constPtrToConstInt@
foreign import ccall unsafe "hs_bindgen_b7f5f49d98fc03a2" hs_bindgen_b7f5f49d98fc03a2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_constPtrToConstInt@
hs_bindgen_b7f5f49d98fc03a2 :: IO (PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt))
hs_bindgen_b7f5f49d98fc03a2 =
  RIP.fromFFIType hs_bindgen_b7f5f49d98fc03a2_base

{-# NOINLINE hs_bindgen_5e25eac293910abb #-}
{-| A const pointer to const int

__C declaration:__ @constPtrToConstInt@

__defined at:__ @manual\/globals.h 69:26@

__exported by:__ @manual\/globals.h@

__unique:__ @test_manualglobals_Example_constPtrToConstInt@
-}
hs_bindgen_5e25eac293910abb :: PtrConst.PtrConst (PtrConst.PtrConst RIP.CInt)
hs_bindgen_5e25eac293910abb =
  RIP.unsafePerformIO hs_bindgen_b7f5f49d98fc03a2

{-# NOINLINE constPtrToConstInt #-}
constPtrToConstInt :: PtrConst.PtrConst RIP.CInt
constPtrToConstInt =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_5e25eac293910abb)

-- __unique:__ @test_manualglobals_Example_get_a@
foreign import ccall unsafe "hs_bindgen_6a66996f25c871b0" hs_bindgen_6a66996f25c871b0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_a@
hs_bindgen_6a66996f25c871b0 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_6a66996f25c871b0 =
  RIP.fromFFIType hs_bindgen_6a66996f25c871b0_base

{-# NOINLINE a #-}
{-| __C declaration:__ @a@

    __defined at:__ @manual\/globals.h 75:12@

    __exported by:__ @manual\/globals.h@
-}
a :: RIP.Ptr RIP.CInt
a = RIP.unsafePerformIO hs_bindgen_6a66996f25c871b0

-- __unique:__ @test_manualglobals_Example_get_a2@
foreign import ccall unsafe "hs_bindgen_69eaf4375e9019f6" hs_bindgen_69eaf4375e9019f6_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_a2@
hs_bindgen_69eaf4375e9019f6 :: IO (PtrConst.PtrConst RIP.CInt)
hs_bindgen_69eaf4375e9019f6 =
  RIP.fromFFIType hs_bindgen_69eaf4375e9019f6_base

{-# NOINLINE hs_bindgen_604b6be28898361f #-}
{-| __C declaration:__ @a2@

    __defined at:__ @manual\/globals.h 76:18@

    __exported by:__ @manual\/globals.h@

    __unique:__ @test_manualglobals_Example_a2@
-}
hs_bindgen_604b6be28898361f :: PtrConst.PtrConst RIP.CInt
hs_bindgen_604b6be28898361f =
  RIP.unsafePerformIO hs_bindgen_69eaf4375e9019f6

{-# NOINLINE a2 #-}
a2 :: RIP.CInt
a2 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_604b6be28898361f)

-- __unique:__ @test_manualglobals_Example_get_b@
foreign import ccall unsafe "hs_bindgen_86029851b6c19d0e" hs_bindgen_86029851b6c19d0e_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_b@
hs_bindgen_86029851b6c19d0e :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_86029851b6c19d0e =
  RIP.fromFFIType hs_bindgen_86029851b6c19d0e_base

{-# NOINLINE b #-}
{-| __C declaration:__ @b@

    __defined at:__ @manual\/globals.h 78:14@

    __exported by:__ @manual\/globals.h@
-}
b :: RIP.Ptr (RIP.Ptr RIP.CInt)
b = RIP.unsafePerformIO hs_bindgen_86029851b6c19d0e

-- __unique:__ @test_manualglobals_Example_get_b2@
foreign import ccall unsafe "hs_bindgen_54ab82d9854a31c0" hs_bindgen_54ab82d9854a31c0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_b2@
hs_bindgen_54ab82d9854a31c0 :: IO (RIP.Ptr (PtrConst.PtrConst RIP.CInt))
hs_bindgen_54ab82d9854a31c0 =
  RIP.fromFFIType hs_bindgen_54ab82d9854a31c0_base

{-# NOINLINE b2 #-}
{-| __C declaration:__ @b2@

    __defined at:__ @manual\/globals.h 79:20@

    __exported by:__ @manual\/globals.h@
-}
b2 :: RIP.Ptr (PtrConst.PtrConst RIP.CInt)
b2 = RIP.unsafePerformIO hs_bindgen_54ab82d9854a31c0

-- __unique:__ @test_manualglobals_Example_get_c@
foreign import ccall unsafe "hs_bindgen_11d5e5406d066dc2" hs_bindgen_11d5e5406d066dc2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_c@
hs_bindgen_11d5e5406d066dc2 :: IO (RIP.Ptr Triplet)
hs_bindgen_11d5e5406d066dc2 =
  RIP.fromFFIType hs_bindgen_11d5e5406d066dc2_base

{-# NOINLINE c #-}
{-| __C declaration:__ @c@

    __defined at:__ @manual\/globals.h 82:16@

    __exported by:__ @manual\/globals.h@
-}
c :: RIP.Ptr Triplet
c = RIP.unsafePerformIO hs_bindgen_11d5e5406d066dc2

-- __unique:__ @test_manualglobals_Example_get_c2@
foreign import ccall unsafe "hs_bindgen_05a85a68bb53daff" hs_bindgen_05a85a68bb53daff_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_c2@
hs_bindgen_05a85a68bb53daff :: IO (PtrConst.PtrConst Triplet)
hs_bindgen_05a85a68bb53daff =
  RIP.fromFFIType hs_bindgen_05a85a68bb53daff_base

{-# NOINLINE hs_bindgen_14cc4924924d5040 #-}
{-| __C declaration:__ @c2@

    __defined at:__ @manual\/globals.h 83:22@

    __exported by:__ @manual\/globals.h@

    __unique:__ @test_manualglobals_Example_c2@
-}
hs_bindgen_14cc4924924d5040 :: PtrConst.PtrConst Triplet
hs_bindgen_14cc4924924d5040 =
  RIP.unsafePerformIO hs_bindgen_05a85a68bb53daff

{-# NOINLINE c2 #-}
c2 :: Triplet
c2 =
  RIP.unsafePerformIO (PtrConst.peek hs_bindgen_14cc4924924d5040)

-- __unique:__ @test_manualglobals_Example_get_d@
foreign import ccall unsafe "hs_bindgen_8fe22549222d7024" hs_bindgen_8fe22549222d7024_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_d@
hs_bindgen_8fe22549222d7024 :: IO (RIP.Ptr List)
hs_bindgen_8fe22549222d7024 =
  RIP.fromFFIType hs_bindgen_8fe22549222d7024_base

{-# NOINLINE d #-}
{-| __C declaration:__ @d@

    __defined at:__ @manual\/globals.h 86:13@

    __exported by:__ @manual\/globals.h@
-}
d :: RIP.Ptr List
d = RIP.unsafePerformIO hs_bindgen_8fe22549222d7024

-- __unique:__ @test_manualglobals_Example_get_d2@
foreign import ccall unsafe "hs_bindgen_3b641c8e9aefc9e3" hs_bindgen_3b641c8e9aefc9e3_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_manualglobals_Example_get_d2@
hs_bindgen_3b641c8e9aefc9e3 :: IO (PtrConst.PtrConst List)
hs_bindgen_3b641c8e9aefc9e3 =
  RIP.fromFFIType hs_bindgen_3b641c8e9aefc9e3_base

{-# NOINLINE d2 #-}
{-| __C declaration:__ @d2@

    __defined at:__ @manual\/globals.h 87:19@

    __exported by:__ @manual\/globals.h@
-}
d2 :: PtrConst.PtrConst List
d2 = RIP.unsafePerformIO hs_bindgen_3b641c8e9aefc9e3
