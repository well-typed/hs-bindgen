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
  , "/* get_simpleGlobal_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_454b1c1a1b7d5b60 (void)"
  , "{"
  , "  return &simpleGlobal;"
  , "}"
  , "/* get_compoundGlobal1_ptr */"
  , "__attribute__ ((const))"
  , "struct config *hs_bindgen_test_globalsglobals_89495849c6dfe834 (void)"
  , "{"
  , "  return &compoundGlobal1;"
  , "}"
  , "/* get_compoundGlobal2_ptr */"
  , "__attribute__ ((const))"
  , "struct inline_struct *hs_bindgen_test_globalsglobals_4b79fc96cf429b3a (void)"
  , "{"
  , "  return &compoundGlobal2;"
  , "}"
  , "/* get_nesInteger_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_e408a99291c13956 (void)"
  , "{"
  , "  return &nesInteger;"
  , "}"
  , "/* get_nesFloating_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_test_globalsglobals_572819a2ae538f9e (void)"
  , "{"
  , "  return &nesFloating;"
  , "}"
  , "/* get_nesString1_ptr */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_test_globalsglobals_b9c5b2253881699a (void)"
  , "{"
  , "  return &nesString1;"
  , "}"
  , "/* get_nesString2_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_test_globalsglobals_cea7b6d95dd81460 (void))[3]"
  , "{"
  , "  return &nesString2;"
  , "}"
  , "/* get_nesCharacter_ptr */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_test_globalsglobals_1e474dd7b5a92e6a (void)"
  , "{"
  , "  return &nesCharacter;"
  , "}"
  , "/* get_nesParen_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_5e7165450285240d (void)"
  , "{"
  , "  return &nesParen;"
  , "}"
  , "/* get_nesUnary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_f00692845b6c8efd (void)"
  , "{"
  , "  return &nesUnary;"
  , "}"
  , "/* get_nesBinary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_d737cdc355737c2b (void)"
  , "{"
  , "  return &nesBinary;"
  , "}"
  , "/* get_nesConditional_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_test_globalsglobals_a8a7382ba67a6c7d (void)"
  , "{"
  , "  return &nesConditional;"
  , "}"
  , "/* get_nesCast_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_test_globalsglobals_ea6b1f97253ae23c (void)"
  , "{"
  , "  return &nesCast;"
  , "}"
  , "/* get_nesCompound_ptr */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_test_globalsglobals_34ed05675f69bccd (void)"
  , "{"
  , "  return &nesCompound;"
  , "}"
  , "/* get_nesInitList_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_test_globalsglobals_9e7c70b133f8e175 (void))[4]"
  , "{"
  , "  return &nesInitList;"
  , "}"
  , "/* get_nesBool_ptr */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_test_globalsglobals_bd8b34eff62c9b12 (void)"
  , "{"
  , "  return &nesBool;"
  , "}"
  , "/* get_streamBinary_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_test_globalsglobals_235c0c54701b0df5 (void))[4096]"
  , "{"
  , "  return &streamBinary;"
  , "}"
  , "/* get_streamBinary_len_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t *hs_bindgen_test_globalsglobals_b1ee511251d335dd (void)"
  , "{"
  , "  return &streamBinary_len;"
  , "}"
  , "/* get_some_global_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct2_t *hs_bindgen_test_globalsglobals_6a47eaca372272ae (void)"
  , "{"
  , "  return &some_global_struct;"
  , "}"
  , "/* get_globalConstant_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_b8fe2f6ed308e786 (void)"
  , "{"
  , "  return &globalConstant;"
  , "}"
  , "/* get_anotherGlobalConstant_ptr */"
  , "__attribute__ ((const))"
  , "ConstInt *hs_bindgen_test_globalsglobals_3f4a524bb162b165 (void)"
  , "{"
  , "  return &anotherGlobalConstant;"
  , "}"
  , "/* get_staticConst_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_834271d76b418960 (void)"
  , "{"
  , "  return &staticConst;"
  , "}"
  , "/* get_classless_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_test_globalsglobals_c98b4d346a0bb607 (void)"
  , "{"
  , "  return &classless;"
  , "}"
  , "/* get_constArray1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_test_globalsglobals_e7a61097e2415261 (void))[4]"
  , "{"
  , "  return &constArray1;"
  , "}"
  , "/* get_constArray2_ptr */"
  , "__attribute__ ((const))"
  , "ConstIntArray *hs_bindgen_test_globalsglobals_5e76637230135838 (void)"
  , "{"
  , "  return &constArray2;"
  , "}"
  , "/* get_constTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple const *hs_bindgen_test_globalsglobals_c7d3a2347092dfe0 (void)"
  , "{"
  , "  return &constTuple;"
  , "}"
  , "/* get_nonConstTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple *hs_bindgen_test_globalsglobals_3639a5c5b63aad84 (void)"
  , "{"
  , "  return &nonConstTuple;"
  , "}"
  , "/* get_ptrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_test_globalsglobals_9fd2e0d3b265dee4 (void)"
  , "{"
  , "  return &ptrToConstInt;"
  , "}"
  , "/* get_constPtrToInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_test_globalsglobals_10cf1344894b0264 (void)"
  , "{"
  , "  return &constPtrToInt;"
  , "}"
  , "/* get_constPtrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_test_globalsglobals_1b236183822e0d24 (void)"
  , "{"
  , "  return &constPtrToConstInt;"
  , "}"
  ]))

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_454b1c1a1b7d5b60" hs_bindgen_test_globalsglobals_454b1c1a1b7d5b60 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE simpleGlobal_ptr #-}

{-| Global variables

__C declaration:__ @simpleGlobal@

__defined at:__ @globals\/globals.h:9:12@

__exported by:__ @globals\/globals.h@
-}
simpleGlobal_ptr :: Ptr.Ptr FC.CInt
simpleGlobal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_454b1c1a1b7d5b60

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_89495849c6dfe834" hs_bindgen_test_globalsglobals_89495849c6dfe834 ::
     IO (Ptr.Ptr Config)

{-# NOINLINE compoundGlobal1_ptr #-}

{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h:16:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1_ptr :: Ptr.Ptr Config
compoundGlobal1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_89495849c6dfe834

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_4b79fc96cf429b3a" hs_bindgen_test_globalsglobals_4b79fc96cf429b3a ::
     IO (Ptr.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2_ptr #-}

{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h:19:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2_ptr :: Ptr.Ptr Inline_struct
compoundGlobal2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_4b79fc96cf429b3a

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_e408a99291c13956" hs_bindgen_test_globalsglobals_e408a99291c13956 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_e408a99291c13956

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_572819a2ae538f9e" hs_bindgen_test_globalsglobals_572819a2ae538f9e ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesFloating_ptr #-}

{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h:36:9@

    __exported by:__ @globals\/globals.h@
-}
nesFloating_ptr :: Ptr.Ptr FC.CFloat
nesFloating_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_572819a2ae538f9e

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b9c5b2253881699a" hs_bindgen_test_globalsglobals_b9c5b2253881699a ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE nesString1_ptr #-}

{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h:38:9@

    __exported by:__ @globals\/globals.h@
-}
nesString1_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
nesString1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b9c5b2253881699a

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_cea7b6d95dd81460" hs_bindgen_test_globalsglobals_cea7b6d95dd81460 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2_ptr #-}

{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h:39:9@

    __exported by:__ @globals\/globals.h@
-}
nesString2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_cea7b6d95dd81460

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_1e474dd7b5a92e6a" hs_bindgen_test_globalsglobals_1e474dd7b5a92e6a ::
     IO (Ptr.Ptr FC.CChar)

{-# NOINLINE nesCharacter_ptr #-}

{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h:40:9@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter_ptr :: Ptr.Ptr FC.CChar
nesCharacter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_1e474dd7b5a92e6a

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_5e7165450285240d" hs_bindgen_test_globalsglobals_5e7165450285240d ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesParen_ptr #-}

{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h:41:9@

    __exported by:__ @globals\/globals.h@
-}
nesParen_ptr :: Ptr.Ptr FC.CInt
nesParen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_5e7165450285240d

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_f00692845b6c8efd" hs_bindgen_test_globalsglobals_f00692845b6c8efd ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesUnary_ptr #-}

{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h:42:9@

    __exported by:__ @globals\/globals.h@
-}
nesUnary_ptr :: Ptr.Ptr FC.CInt
nesUnary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_f00692845b6c8efd

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_d737cdc355737c2b" hs_bindgen_test_globalsglobals_d737cdc355737c2b ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesBinary_ptr #-}

{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h:43:9@

    __exported by:__ @globals\/globals.h@
-}
nesBinary_ptr :: Ptr.Ptr FC.CInt
nesBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_d737cdc355737c2b

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_a8a7382ba67a6c7d" hs_bindgen_test_globalsglobals_a8a7382ba67a6c7d ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesConditional_ptr #-}

{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h:44:9@

    __exported by:__ @globals\/globals.h@
-}
nesConditional_ptr :: Ptr.Ptr FC.CInt
nesConditional_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_a8a7382ba67a6c7d

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_ea6b1f97253ae23c" hs_bindgen_test_globalsglobals_ea6b1f97253ae23c ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesCast_ptr #-}

{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h:45:9@

    __exported by:__ @globals\/globals.h@
-}
nesCast_ptr :: Ptr.Ptr FC.CFloat
nesCast_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_ea6b1f97253ae23c

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_34ed05675f69bccd" hs_bindgen_test_globalsglobals_34ed05675f69bccd ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE nesCompound_ptr #-}

{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h:46:9@

    __exported by:__ @globals\/globals.h@
-}
nesCompound_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
nesCompound_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_34ed05675f69bccd

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_9e7c70b133f8e175" hs_bindgen_test_globalsglobals_9e7c70b133f8e175 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList_ptr #-}

{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h:47:9@

    __exported by:__ @globals\/globals.h@
-}
nesInitList_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_9e7c70b133f8e175

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_bd8b34eff62c9b12" hs_bindgen_test_globalsglobals_bd8b34eff62c9b12 ::
     IO (Ptr.Ptr FC.CBool)

{-# NOINLINE nesBool_ptr #-}

{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h:48:9@

    __exported by:__ @globals\/globals.h@
-}
nesBool_ptr :: Ptr.Ptr FC.CBool
nesBool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_bd8b34eff62c9b12

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_235c0c54701b0df5" hs_bindgen_test_globalsglobals_235c0c54701b0df5 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_235c0c54701b0df5

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b1ee511251d335dd" hs_bindgen_test_globalsglobals_b1ee511251d335dd ::
     IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len_ptr #-}

{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h:404:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len_ptr :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b1ee511251d335dd

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_6a47eaca372272ae" hs_bindgen_test_globalsglobals_6a47eaca372272ae ::
     IO (Ptr.Ptr Struct2_t)

{-# NOINLINE some_global_struct_ptr #-}

{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h:425:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct_ptr :: Ptr.Ptr Struct2_t
some_global_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_6a47eaca372272ae

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_b8fe2f6ed308e786" hs_bindgen_test_globalsglobals_b8fe2f6ed308e786 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_b8fe2f6ed308e786

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek globalConstant_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_3f4a524bb162b165" hs_bindgen_test_globalsglobals_3f4a524bb162b165 ::
     IO (Ptr.Ptr ConstInt)

{-# NOINLINE anotherGlobalConstant_ptr #-}

{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @globals\/globals.h:449:17@

    __exported by:__ @globals\/globals.h@
-}
anotherGlobalConstant_ptr :: Ptr.Ptr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_3f4a524bb162b165

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_834271d76b418960" hs_bindgen_test_globalsglobals_834271d76b418960 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_834271d76b418960

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_c98b4d346a0bb607" hs_bindgen_test_globalsglobals_c98b4d346a0bb607 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE classless_ptr #-}

{-| No storage class specified

__C declaration:__ @classless@

__defined at:__ @globals\/globals.h:457:11@

__exported by:__ @globals\/globals.h@
-}
classless_ptr :: Ptr.Ptr FC.CInt
classless_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_c98b4d346a0bb607

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_e7a61097e2415261" hs_bindgen_test_globalsglobals_e7a61097e2415261 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))

{-# NOINLINE constArray1_ptr #-}

{-| A an array of size 4 containing constant integers

__C declaration:__ @constArray1@

__defined at:__ @globals\/globals.h:460:18@

__exported by:__ @globals\/globals.h@
-}
constArray1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_e7a61097e2415261

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constArray1_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_5e76637230135838" hs_bindgen_test_globalsglobals_5e76637230135838 ::
     IO (Ptr.Ptr ConstIntArray)

{-# NOINLINE constArray2_ptr #-}

{-| __C declaration:__ @constArray2@

    __defined at:__ @globals\/globals.h:464:22@

    __exported by:__ @globals\/globals.h@
-}
constArray2_ptr :: Ptr.Ptr ConstIntArray
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_5e76637230135838

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_c7d3a2347092dfe0" hs_bindgen_test_globalsglobals_c7d3a2347092dfe0 ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE constTuple_ptr #-}

{-| A constant tuple

__C declaration:__ @constTuple@

__defined at:__ @globals\/globals.h:468:27@

__exported by:__ @globals\/globals.h@
-}
constTuple_ptr :: Ptr.Ptr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_c7d3a2347092dfe0

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_3639a5c5b63aad84" hs_bindgen_test_globalsglobals_3639a5c5b63aad84 ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE nonConstTuple_ptr #-}

{-| A non-constant tuple with a constant member

__C declaration:__ @nonConstTuple@

__defined at:__ @globals\/globals.h:470:21@

__exported by:__ @globals\/globals.h@
-}
nonConstTuple_ptr :: Ptr.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_3639a5c5b63aad84

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_9fd2e0d3b265dee4" hs_bindgen_test_globalsglobals_9fd2e0d3b265dee4 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE ptrToConstInt_ptr #-}

{-| A pointer to const int

__C declaration:__ @ptrToConstInt@

__defined at:__ @globals\/globals.h:473:20@

__exported by:__ @globals\/globals.h@
-}
ptrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_9fd2e0d3b265dee4

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_10cf1344894b0264" hs_bindgen_test_globalsglobals_10cf1344894b0264 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToInt_ptr #-}

{-| A const pointer to int

__C declaration:__ @constPtrToInt@

__defined at:__ @globals\/globals.h:475:20@

__exported by:__ @globals\/globals.h@
-}
constPtrToInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_10cf1344894b0264

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: Ptr.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

foreign import ccall unsafe "hs_bindgen_test_globalsglobals_1b236183822e0d24" hs_bindgen_test_globalsglobals_1b236183822e0d24 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToConstInt_ptr #-}

{-| A const pointer to const int

__C declaration:__ @constPtrToConstInt@

__defined at:__ @globals\/globals.h:477:26@

__exported by:__ @globals\/globals.h@
-}
constPtrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globalsglobals_1b236183822e0d24

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: Ptr.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
