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
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <globals/globals.h>"
  , "/* Example_get_simpleGlobal_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_bb539f9386d11333 (void)"
  , "{"
  , "  return &simpleGlobal;"
  , "}"
  , "/* Example_get_compoundGlobal1_ptr */"
  , "__attribute__ ((const))"
  , "struct config *hs_bindgen_test_globalsglobals_4bc4886b619fa8b8 (void)"
  , "{"
  , "  return &compoundGlobal1;"
  , "}"
  , "/* Example_get_compoundGlobal2_ptr */"
  , "__attribute__ ((const))"
  , "struct inline_struct *hs_bindgen_test_globalsglobals_27f7a88c587d58d2 (void)"
  , "{"
  , "  return &compoundGlobal2;"
  , "}"
  , "/* Example_get_nesInteger_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_ec99dd72e149151f (void)"
  , "{"
  , "  return &nesInteger;"
  , "}"
  , "/* Example_get_nesFloating_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_test_globalsglobals_c50544322aea215d (void)"
  , "{"
  , "  return &nesFloating;"
  , "}"
  , "/* Example_get_nesString1_ptr */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_test_globalsglobals_dfcf8e48c44e3881 (void)"
  , "{"
  , "  return &nesString1;"
  , "}"
  , "/* Example_get_nesString2_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_globalsglobals_e526691d0ef5d801 (void))[3]"
  , "{"
  , "  return &nesString2;"
  , "}"
  , "/* Example_get_nesCharacter_ptr */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_test_globalsglobals_b12f13809c62cecc (void)"
  , "{"
  , "  return &nesCharacter;"
  , "}"
  , "/* Example_get_nesParen_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_ae2df43544141229 (void)"
  , "{"
  , "  return &nesParen;"
  , "}"
  , "/* Example_get_nesUnary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_0f90089e292fe84e (void)"
  , "{"
  , "  return &nesUnary;"
  , "}"
  , "/* Example_get_nesBinary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_a8107f4d1c51728a (void)"
  , "{"
  , "  return &nesBinary;"
  , "}"
  , "/* Example_get_nesConditional_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_ad7a5fab06cac7bd (void)"
  , "{"
  , "  return &nesConditional;"
  , "}"
  , "/* Example_get_nesCast_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_test_globalsglobals_ff382ce7e81ef0ac (void)"
  , "{"
  , "  return &nesCast;"
  , "}"
  , "/* Example_get_nesCompound_ptr */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_test_globalsglobals_ac4df2d797f72985 (void)"
  , "{"
  , "  return &nesCompound;"
  , "}"
  , "/* Example_get_nesInitList_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_test_globalsglobals_74bd31fce21f55b9 (void))[4]"
  , "{"
  , "  return &nesInitList;"
  , "}"
  , "/* Example_get_nesBool_ptr */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_test_globalsglobals_2bc7c46d0958ab21 (void)"
  , "{"
  , "  return &nesBool;"
  , "}"
  , "/* Example_get_streamBinary_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_test_globalsglobals_539645480ec6fd7e (void))[4096]"
  , "{"
  , "  return &streamBinary;"
  , "}"
  , "/* Example_get_streamBinary_len_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t *hs_bindgen_test_globalsglobals_191ba13805ee6f1b (void)"
  , "{"
  , "  return &streamBinary_len;"
  , "}"
  , "/* Example_get_some_global_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct2_t *hs_bindgen_test_globalsglobals_ea26b75b89493ccf (void)"
  , "{"
  , "  return &some_global_struct;"
  , "}"
  , "/* Example_get_globalConstant_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_fce5826c086543e6 (void)"
  , "{"
  , "  return &globalConstant;"
  , "}"
  , "/* Example_get_anotherGlobalConstant_ptr */"
  , "__attribute__ ((const))"
  , "ConstInt *hs_bindgen_test_globalsglobals_cebb2145e274f4ab (void)"
  , "{"
  , "  return &anotherGlobalConstant;"
  , "}"
  , "/* Example_get_staticConst_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_c9a2e80840613f3f (void)"
  , "{"
  , "  return &staticConst;"
  , "}"
  , "/* Example_get_classless_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_5551245e770e6acc (void)"
  , "{"
  , "  return &classless;"
  , "}"
  , "/* Example_get_constArray1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_globalsglobals_81c31e5942087cd1 (void))[4]"
  , "{"
  , "  return &constArray1;"
  , "}"
  , "/* Example_get_constArray2_ptr */"
  , "__attribute__ ((const))"
  , "ConstIntArray *hs_bindgen_test_globalsglobals_b929bffa7ab86046 (void)"
  , "{"
  , "  return &constArray2;"
  , "}"
  , "/* Example_get_constTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple const *hs_bindgen_test_globalsglobals_b9ebdd39b87caa0c (void)"
  , "{"
  , "  return &constTuple;"
  , "}"
  , "/* Example_get_nonConstTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple *hs_bindgen_test_globalsglobals_67839eb6dcfd3bef (void)"
  , "{"
  , "  return &nonConstTuple;"
  , "}"
  , "/* Example_get_ptrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_test_globalsglobals_4b549e8603212896 (void)"
  , "{"
  , "  return &ptrToConstInt;"
  , "}"
  , "/* Example_get_constPtrToInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_test_globalsglobals_70f5ae2eca134294 (void)"
  , "{"
  , "  return &constPtrToInt;"
  , "}"
  , "/* Example_get_constPtrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_test_globalsglobals_c78ea972eaf7b21f (void)"
  , "{"
  , "  return &constPtrToConstInt;"
  , "}"
  ]))

{-| __unique:__ @Example_get_simpleGlobal_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_bb539f9386d11333" hs_bindgen_test_globalsglobals_bb539f9386d11333 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE simpleGlobal_ptr #-}

{-| Global variables

__C declaration:__ @simpleGlobal@

__defined at:__ @globals\/globals.h:9:12@

__exported by:__ @globals\/globals.h@
-}
simpleGlobal_ptr :: Ptr.Ptr FC.CInt
simpleGlobal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_bb539f9386d11333

{-| __unique:__ @Example_get_compoundGlobal1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_4bc4886b619fa8b8" hs_bindgen_test_globalsglobals_4bc4886b619fa8b8 ::
     IO (Ptr.Ptr Config)

{-# NOINLINE compoundGlobal1_ptr #-}

{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h:16:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1_ptr :: Ptr.Ptr Config
compoundGlobal1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_4bc4886b619fa8b8

{-| __unique:__ @Example_get_compoundGlobal2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_27f7a88c587d58d2" hs_bindgen_test_globalsglobals_27f7a88c587d58d2 ::
     IO (Ptr.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2_ptr #-}

{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h:19:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2_ptr :: Ptr.Ptr Inline_struct
compoundGlobal2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_27f7a88c587d58d2

{-| __unique:__ @Example_get_nesInteger_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ec99dd72e149151f" hs_bindgen_test_globalsglobals_ec99dd72e149151f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesInteger_ptr #-}

{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

__C declaration:__ @nesInteger@

__defined at:__ @globals\/globals.h:35:9@

__exported by:__ @globals\/globals.h@
-}
nesInteger_ptr :: Ptr.Ptr FC.CInt
nesInteger_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ec99dd72e149151f

{-| __unique:__ @Example_get_nesFloating_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_c50544322aea215d" hs_bindgen_test_globalsglobals_c50544322aea215d ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesFloating_ptr #-}

{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h:36:9@

    __exported by:__ @globals\/globals.h@
-}
nesFloating_ptr :: Ptr.Ptr FC.CFloat
nesFloating_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_c50544322aea215d

{-| __unique:__ @Example_get_nesString1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_dfcf8e48c44e3881" hs_bindgen_test_globalsglobals_dfcf8e48c44e3881 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE nesString1_ptr #-}

{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h:38:9@

    __exported by:__ @globals\/globals.h@
-}
nesString1_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
nesString1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_dfcf8e48c44e3881

{-| __unique:__ @Example_get_nesString2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_e526691d0ef5d801" hs_bindgen_test_globalsglobals_e526691d0ef5d801 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2_ptr #-}

{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h:39:9@

    __exported by:__ @globals\/globals.h@
-}
nesString2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_e526691d0ef5d801

{-| __unique:__ @Example_get_nesCharacter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b12f13809c62cecc" hs_bindgen_test_globalsglobals_b12f13809c62cecc ::
     IO (Ptr.Ptr FC.CChar)

{-# NOINLINE nesCharacter_ptr #-}

{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h:40:9@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter_ptr :: Ptr.Ptr FC.CChar
nesCharacter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b12f13809c62cecc

{-| __unique:__ @Example_get_nesParen_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ae2df43544141229" hs_bindgen_test_globalsglobals_ae2df43544141229 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesParen_ptr #-}

{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h:41:9@

    __exported by:__ @globals\/globals.h@
-}
nesParen_ptr :: Ptr.Ptr FC.CInt
nesParen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ae2df43544141229

{-| __unique:__ @Example_get_nesUnary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_0f90089e292fe84e" hs_bindgen_test_globalsglobals_0f90089e292fe84e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesUnary_ptr #-}

{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h:42:9@

    __exported by:__ @globals\/globals.h@
-}
nesUnary_ptr :: Ptr.Ptr FC.CInt
nesUnary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_0f90089e292fe84e

{-| __unique:__ @Example_get_nesBinary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_a8107f4d1c51728a" hs_bindgen_test_globalsglobals_a8107f4d1c51728a ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesBinary_ptr #-}

{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h:43:9@

    __exported by:__ @globals\/globals.h@
-}
nesBinary_ptr :: Ptr.Ptr FC.CInt
nesBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_a8107f4d1c51728a

{-| __unique:__ @Example_get_nesConditional_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ad7a5fab06cac7bd" hs_bindgen_test_globalsglobals_ad7a5fab06cac7bd ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesConditional_ptr #-}

{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h:44:9@

    __exported by:__ @globals\/globals.h@
-}
nesConditional_ptr :: Ptr.Ptr FC.CInt
nesConditional_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ad7a5fab06cac7bd

{-| __unique:__ @Example_get_nesCast_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ff382ce7e81ef0ac" hs_bindgen_test_globalsglobals_ff382ce7e81ef0ac ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesCast_ptr #-}

{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h:45:9@

    __exported by:__ @globals\/globals.h@
-}
nesCast_ptr :: Ptr.Ptr FC.CFloat
nesCast_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ff382ce7e81ef0ac

{-| __unique:__ @Example_get_nesCompound_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ac4df2d797f72985" hs_bindgen_test_globalsglobals_ac4df2d797f72985 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE nesCompound_ptr #-}

{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h:46:9@

    __exported by:__ @globals\/globals.h@
-}
nesCompound_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
nesCompound_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ac4df2d797f72985

{-| __unique:__ @Example_get_nesInitList_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_74bd31fce21f55b9" hs_bindgen_test_globalsglobals_74bd31fce21f55b9 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList_ptr #-}

{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h:47:9@

    __exported by:__ @globals\/globals.h@
-}
nesInitList_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_74bd31fce21f55b9

{-| __unique:__ @Example_get_nesBool_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_2bc7c46d0958ab21" hs_bindgen_test_globalsglobals_2bc7c46d0958ab21 ::
     IO (Ptr.Ptr FC.CBool)

{-# NOINLINE nesBool_ptr #-}

{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h:48:9@

    __exported by:__ @globals\/globals.h@
-}
nesBool_ptr :: Ptr.Ptr FC.CBool
nesBool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_2bc7c46d0958ab21

{-| __unique:__ @Example_get_streamBinary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_539645480ec6fd7e" hs_bindgen_test_globalsglobals_539645480ec6fd7e ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE streamBinary_ptr #-}

{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

__C declaration:__ @streamBinary@

__defined at:__ @globals\/globals.h:60:9@

__exported by:__ @globals\/globals.h@
-}
streamBinary_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)
streamBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_539645480ec6fd7e

{-| __unique:__ @Example_get_streamBinary_len_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_191ba13805ee6f1b" hs_bindgen_test_globalsglobals_191ba13805ee6f1b ::
     IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len_ptr #-}

{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h:404:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len_ptr :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_191ba13805ee6f1b

{-| __unique:__ @Example_get_some_global_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ea26b75b89493ccf" hs_bindgen_test_globalsglobals_ea26b75b89493ccf ::
     IO (Ptr.Ptr Struct2_t)

{-# NOINLINE some_global_struct_ptr #-}

{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h:425:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct_ptr :: Ptr.Ptr Struct2_t
some_global_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ea26b75b89493ccf

{-| __unique:__ @Example_get_globalConstant_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_fce5826c086543e6" hs_bindgen_test_globalsglobals_fce5826c086543e6 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE globalConstant_ptr #-}

{-| Constant

  Although this is a constant, we don't expect an initializer (since it's `extern`).

__C declaration:__ @globalConstant@

__defined at:__ @globals\/globals.h:445:18@

__exported by:__ @globals\/globals.h@
-}
globalConstant_ptr :: Ptr.Ptr FC.CInt
globalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_fce5826c086543e6

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek globalConstant_ptr)

{-| __unique:__ @Example_get_anotherGlobalConstant_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_cebb2145e274f4ab" hs_bindgen_test_globalsglobals_cebb2145e274f4ab ::
     IO (Ptr.Ptr ConstInt)

{-# NOINLINE anotherGlobalConstant_ptr #-}

{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @globals\/globals.h:449:17@

    __exported by:__ @globals\/globals.h@
-}
anotherGlobalConstant_ptr :: Ptr.Ptr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_cebb2145e274f4ab

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

{-| __unique:__ @Example_get_staticConst_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_c9a2e80840613f3f" hs_bindgen_test_globalsglobals_c9a2e80840613f3f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE staticConst_ptr #-}

{-| Constant, but local to the file

  Unlike with `extern`, in this we _do_ expect an initializer.

__C declaration:__ @staticConst@

__defined at:__ @globals\/globals.h:454:18@

__exported by:__ @globals\/globals.h@
-}
staticConst_ptr :: Ptr.Ptr FC.CInt
staticConst_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_c9a2e80840613f3f

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

{-| __unique:__ @Example_get_classless_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_5551245e770e6acc" hs_bindgen_test_globalsglobals_5551245e770e6acc ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE classless_ptr #-}

{-| No storage class specified

__C declaration:__ @classless@

__defined at:__ @globals\/globals.h:457:11@

__exported by:__ @globals\/globals.h@
-}
classless_ptr :: Ptr.Ptr FC.CInt
classless_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_5551245e770e6acc

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

{-| __unique:__ @Example_get_constArray1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_81c31e5942087cd1" hs_bindgen_test_globalsglobals_81c31e5942087cd1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))

{-# NOINLINE constArray1_ptr #-}

{-| A an array of size 4 containing constant integers

__C declaration:__ @constArray1@

__defined at:__ @globals\/globals.h:460:18@

__exported by:__ @globals\/globals.h@
-}
constArray1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_81c31e5942087cd1

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constArray1_ptr)

{-| __unique:__ @Example_get_constArray2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b929bffa7ab86046" hs_bindgen_test_globalsglobals_b929bffa7ab86046 ::
     IO (Ptr.Ptr ConstIntArray)

{-# NOINLINE constArray2_ptr #-}

{-| __C declaration:__ @constArray2@

    __defined at:__ @globals\/globals.h:464:22@

    __exported by:__ @globals\/globals.h@
-}
constArray2_ptr :: Ptr.Ptr ConstIntArray
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b929bffa7ab86046

{-| __unique:__ @Example_get_constTuple_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b9ebdd39b87caa0c" hs_bindgen_test_globalsglobals_b9ebdd39b87caa0c ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE constTuple_ptr #-}

{-| A constant tuple

__C declaration:__ @constTuple@

__defined at:__ @globals\/globals.h:468:27@

__exported by:__ @globals\/globals.h@
-}
constTuple_ptr :: Ptr.Ptr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b9ebdd39b87caa0c

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

{-| __unique:__ @Example_get_nonConstTuple_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_67839eb6dcfd3bef" hs_bindgen_test_globalsglobals_67839eb6dcfd3bef ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE nonConstTuple_ptr #-}

{-| A non-constant tuple with a constant member

__C declaration:__ @nonConstTuple@

__defined at:__ @globals\/globals.h:470:21@

__exported by:__ @globals\/globals.h@
-}
nonConstTuple_ptr :: Ptr.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_67839eb6dcfd3bef

{-| __unique:__ @Example_get_ptrToConstInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_4b549e8603212896" hs_bindgen_test_globalsglobals_4b549e8603212896 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE ptrToConstInt_ptr #-}

{-| A pointer to const int

__C declaration:__ @ptrToConstInt@

__defined at:__ @globals\/globals.h:473:20@

__exported by:__ @globals\/globals.h@
-}
ptrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_4b549e8603212896

{-| __unique:__ @Example_get_constPtrToInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_70f5ae2eca134294" hs_bindgen_test_globalsglobals_70f5ae2eca134294 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToInt_ptr #-}

{-| A const pointer to int

__C declaration:__ @constPtrToInt@

__defined at:__ @globals\/globals.h:475:20@

__exported by:__ @globals\/globals.h@
-}
constPtrToInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_70f5ae2eca134294

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: Ptr.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

{-| __unique:__ @Example_get_constPtrToConstInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_test_globalsglobals_c78ea972eaf7b21f" hs_bindgen_test_globalsglobals_c78ea972eaf7b21f ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToConstInt_ptr #-}

{-| A const pointer to const int

__C declaration:__ @constPtrToConstInt@

__defined at:__ @globals\/globals.h:477:26@

__exported by:__ @globals\/globals.h@
-}
constPtrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_c78ea972eaf7b21f

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: Ptr.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
