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
  , "/* test_globalsglobals_Example_get_simpleGlobal_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_714e2053c32bb476 (void)"
  , "{"
  , "  return &simpleGlobal;"
  , "}"
  , "/* test_globalsglobals_Example_get_compoundGlobal1_ptr */"
  , "__attribute__ ((const))"
  , "struct config *hs_bindgen_67e48c5e13ca2c60 (void)"
  , "{"
  , "  return &compoundGlobal1;"
  , "}"
  , "/* test_globalsglobals_Example_get_compoundGlobal2_ptr */"
  , "__attribute__ ((const))"
  , "struct inline_struct *hs_bindgen_f26f5d6ef3b76089 (void)"
  , "{"
  , "  return &compoundGlobal2;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesInteger_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_3ebebd14325934b9 (void)"
  , "{"
  , "  return &nesInteger;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesFloating_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_10b443be437175ac (void)"
  , "{"
  , "  return &nesFloating;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesString1_ptr */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_5cc8248fbb1c759a (void)"
  , "{"
  , "  return &nesString1;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesString2_ptr */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_553e972cf96f76d8 (void))[3]"
  , "{"
  , "  return &nesString2;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCharacter_ptr */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_9d3773e854e51f24 (void)"
  , "{"
  , "  return &nesCharacter;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesParen_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_a453d5edd9071d44 (void)"
  , "{"
  , "  return &nesParen;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesUnary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_bad560390cc25eb6 (void)"
  , "{"
  , "  return &nesUnary;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesBinary_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_b27e845110f4bbec (void)"
  , "{"
  , "  return &nesBinary;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesConditional_ptr */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_34b54bf36a1d379e (void)"
  , "{"
  , "  return &nesConditional;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCast_ptr */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_c61f871f8564e025 (void)"
  , "{"
  , "  return &nesCast;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCompound_ptr */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_048b3e5b4043e865 (void)"
  , "{"
  , "  return &nesCompound;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesInitList_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_b38ff22e5052f65a (void))[4]"
  , "{"
  , "  return &nesInitList;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesBool_ptr */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_b91bd5866e3f3d29 (void)"
  , "{"
  , "  return &nesBool;"
  , "}"
  , "/* test_globalsglobals_Example_get_streamBinary_ptr */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_cc754e9476d41d9c (void))[4096]"
  , "{"
  , "  return &streamBinary;"
  , "}"
  , "/* test_globalsglobals_Example_get_streamBinary_len_ptr */"
  , "__attribute__ ((const))"
  , "uint32_t *hs_bindgen_070dcbfed009198d (void)"
  , "{"
  , "  return &streamBinary_len;"
  , "}"
  , "/* test_globalsglobals_Example_get_some_global_struct_ptr */"
  , "__attribute__ ((const))"
  , "struct2_t *hs_bindgen_799ae43fda9906f5 (void)"
  , "{"
  , "  return &some_global_struct;"
  , "}"
  , "/* test_globalsglobals_Example_get_globalConstant_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_8c2513f5f7e3236e (void)"
  , "{"
  , "  return &globalConstant;"
  , "}"
  , "/* test_globalsglobals_Example_get_anotherGlobalConstant_ptr */"
  , "__attribute__ ((const))"
  , "ConstInt *hs_bindgen_8123602967122676 (void)"
  , "{"
  , "  return &anotherGlobalConstant;"
  , "}"
  , "/* test_globalsglobals_Example_get_staticConst_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_d04ef3b41afccd4c (void)"
  , "{"
  , "  return &staticConst;"
  , "}"
  , "/* test_globalsglobals_Example_get_classless_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *hs_bindgen_5d93bd707e83de07 (void)"
  , "{"
  , "  return &classless;"
  , "}"
  , "/* test_globalsglobals_Example_get_constArray1_ptr */"
  , "__attribute__ ((const))"
  , "signed int const (*hs_bindgen_7779e72b6ab72de1 (void))[4]"
  , "{"
  , "  return &constArray1;"
  , "}"
  , "/* test_globalsglobals_Example_get_constArray2_ptr */"
  , "__attribute__ ((const))"
  , "ConstIntArray *hs_bindgen_fed813bd4083c3c5 (void)"
  , "{"
  , "  return &constArray2;"
  , "}"
  , "/* test_globalsglobals_Example_get_constTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple const *hs_bindgen_07cd8b2ac394e36c (void)"
  , "{"
  , "  return &constTuple;"
  , "}"
  , "/* test_globalsglobals_Example_get_nonConstTuple_ptr */"
  , "__attribute__ ((const))"
  , "struct tuple *hs_bindgen_d5ea9aa1a5cbcedb (void)"
  , "{"
  , "  return &nonConstTuple;"
  , "}"
  , "/* test_globalsglobals_Example_get_ptrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const **hs_bindgen_f167ac4f659d037d (void)"
  , "{"
  , "  return &ptrToConstInt;"
  , "}"
  , "/* test_globalsglobals_Example_get_constPtrToInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int *const *hs_bindgen_8376621614875e3d (void)"
  , "{"
  , "  return &constPtrToInt;"
  , "}"
  , "/* test_globalsglobals_Example_get_constPtrToConstInt_ptr */"
  , "__attribute__ ((const))"
  , "signed int const *const *hs_bindgen_afb4f6b5c6ec422e (void)"
  , "{"
  , "  return &constPtrToConstInt;"
  , "}"
  ]))

{-| __unique:__ @test_globalsglobals_Example_get_simpleGlobal_ptr@
-}
foreign import ccall unsafe "hs_bindgen_714e2053c32bb476" hs_bindgen_714e2053c32bb476 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE simpleGlobal_ptr #-}

{-| Global variables

__C declaration:__ @simpleGlobal@

__defined at:__ @globals\/globals.h:9:12@

__exported by:__ @globals\/globals.h@
-}
simpleGlobal_ptr :: Ptr.Ptr FC.CInt
simpleGlobal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_714e2053c32bb476

{-| __unique:__ @test_globalsglobals_Example_get_compoundGlobal1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_67e48c5e13ca2c60" hs_bindgen_67e48c5e13ca2c60 ::
     IO (Ptr.Ptr Config)

{-# NOINLINE compoundGlobal1_ptr #-}

{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h:16:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1_ptr :: Ptr.Ptr Config
compoundGlobal1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_67e48c5e13ca2c60

{-| __unique:__ @test_globalsglobals_Example_get_compoundGlobal2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_f26f5d6ef3b76089" hs_bindgen_f26f5d6ef3b76089 ::
     IO (Ptr.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2_ptr #-}

{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h:19:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2_ptr :: Ptr.Ptr Inline_struct
compoundGlobal2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f26f5d6ef3b76089

{-| __unique:__ @test_globalsglobals_Example_get_nesInteger_ptr@
-}
foreign import ccall unsafe "hs_bindgen_3ebebd14325934b9" hs_bindgen_3ebebd14325934b9 ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_3ebebd14325934b9

{-| __unique:__ @test_globalsglobals_Example_get_nesFloating_ptr@
-}
foreign import ccall unsafe "hs_bindgen_10b443be437175ac" hs_bindgen_10b443be437175ac ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesFloating_ptr #-}

{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h:36:9@

    __exported by:__ @globals\/globals.h@
-}
nesFloating_ptr :: Ptr.Ptr FC.CFloat
nesFloating_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_10b443be437175ac

{-| __unique:__ @test_globalsglobals_Example_get_nesString1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_5cc8248fbb1c759a" hs_bindgen_5cc8248fbb1c759a ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE nesString1_ptr #-}

{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h:38:9@

    __exported by:__ @globals\/globals.h@
-}
nesString1_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
nesString1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5cc8248fbb1c759a

{-| __unique:__ @test_globalsglobals_Example_get_nesString2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_553e972cf96f76d8" hs_bindgen_553e972cf96f76d8 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2_ptr #-}

{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h:39:9@

    __exported by:__ @globals\/globals.h@
-}
nesString2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_553e972cf96f76d8

{-| __unique:__ @test_globalsglobals_Example_get_nesCharacter_ptr@
-}
foreign import ccall unsafe "hs_bindgen_9d3773e854e51f24" hs_bindgen_9d3773e854e51f24 ::
     IO (Ptr.Ptr FC.CChar)

{-# NOINLINE nesCharacter_ptr #-}

{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h:40:9@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter_ptr :: Ptr.Ptr FC.CChar
nesCharacter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9d3773e854e51f24

{-| __unique:__ @test_globalsglobals_Example_get_nesParen_ptr@
-}
foreign import ccall unsafe "hs_bindgen_a453d5edd9071d44" hs_bindgen_a453d5edd9071d44 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesParen_ptr #-}

{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h:41:9@

    __exported by:__ @globals\/globals.h@
-}
nesParen_ptr :: Ptr.Ptr FC.CInt
nesParen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_a453d5edd9071d44

{-| __unique:__ @test_globalsglobals_Example_get_nesUnary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_bad560390cc25eb6" hs_bindgen_bad560390cc25eb6 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesUnary_ptr #-}

{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h:42:9@

    __exported by:__ @globals\/globals.h@
-}
nesUnary_ptr :: Ptr.Ptr FC.CInt
nesUnary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_bad560390cc25eb6

{-| __unique:__ @test_globalsglobals_Example_get_nesBinary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_b27e845110f4bbec" hs_bindgen_b27e845110f4bbec ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesBinary_ptr #-}

{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h:43:9@

    __exported by:__ @globals\/globals.h@
-}
nesBinary_ptr :: Ptr.Ptr FC.CInt
nesBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b27e845110f4bbec

{-| __unique:__ @test_globalsglobals_Example_get_nesConditional_ptr@
-}
foreign import ccall unsafe "hs_bindgen_34b54bf36a1d379e" hs_bindgen_34b54bf36a1d379e ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesConditional_ptr #-}

{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h:44:9@

    __exported by:__ @globals\/globals.h@
-}
nesConditional_ptr :: Ptr.Ptr FC.CInt
nesConditional_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_34b54bf36a1d379e

{-| __unique:__ @test_globalsglobals_Example_get_nesCast_ptr@
-}
foreign import ccall unsafe "hs_bindgen_c61f871f8564e025" hs_bindgen_c61f871f8564e025 ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesCast_ptr #-}

{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h:45:9@

    __exported by:__ @globals\/globals.h@
-}
nesCast_ptr :: Ptr.Ptr FC.CFloat
nesCast_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c61f871f8564e025

{-| __unique:__ @test_globalsglobals_Example_get_nesCompound_ptr@
-}
foreign import ccall unsafe "hs_bindgen_048b3e5b4043e865" hs_bindgen_048b3e5b4043e865 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE nesCompound_ptr #-}

{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h:46:9@

    __exported by:__ @globals\/globals.h@
-}
nesCompound_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
nesCompound_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_048b3e5b4043e865

{-| __unique:__ @test_globalsglobals_Example_get_nesInitList_ptr@
-}
foreign import ccall unsafe "hs_bindgen_b38ff22e5052f65a" hs_bindgen_b38ff22e5052f65a ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList_ptr #-}

{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h:47:9@

    __exported by:__ @globals\/globals.h@
-}
nesInitList_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b38ff22e5052f65a

{-| __unique:__ @test_globalsglobals_Example_get_nesBool_ptr@
-}
foreign import ccall unsafe "hs_bindgen_b91bd5866e3f3d29" hs_bindgen_b91bd5866e3f3d29 ::
     IO (Ptr.Ptr FC.CBool)

{-# NOINLINE nesBool_ptr #-}

{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h:48:9@

    __exported by:__ @globals\/globals.h@
-}
nesBool_ptr :: Ptr.Ptr FC.CBool
nesBool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b91bd5866e3f3d29

{-| __unique:__ @test_globalsglobals_Example_get_streamBinary_ptr@
-}
foreign import ccall unsafe "hs_bindgen_cc754e9476d41d9c" hs_bindgen_cc754e9476d41d9c ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_cc754e9476d41d9c

{-| __unique:__ @test_globalsglobals_Example_get_streamBinary_len_ptr@
-}
foreign import ccall unsafe "hs_bindgen_070dcbfed009198d" hs_bindgen_070dcbfed009198d ::
     IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len_ptr #-}

{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h:404:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len_ptr :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_070dcbfed009198d

{-| __unique:__ @test_globalsglobals_Example_get_some_global_struct_ptr@
-}
foreign import ccall unsafe "hs_bindgen_799ae43fda9906f5" hs_bindgen_799ae43fda9906f5 ::
     IO (Ptr.Ptr Struct2_t)

{-# NOINLINE some_global_struct_ptr #-}

{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h:425:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct_ptr :: Ptr.Ptr Struct2_t
some_global_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_799ae43fda9906f5

{-| __unique:__ @test_globalsglobals_Example_get_globalConstant_ptr@
-}
foreign import ccall unsafe "hs_bindgen_8c2513f5f7e3236e" hs_bindgen_8c2513f5f7e3236e ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8c2513f5f7e3236e

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek globalConstant_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_anotherGlobalConstant_ptr@
-}
foreign import ccall unsafe "hs_bindgen_8123602967122676" hs_bindgen_8123602967122676 ::
     IO (Ptr.Ptr ConstInt)

{-# NOINLINE anotherGlobalConstant_ptr #-}

{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @globals\/globals.h:449:17@

    __exported by:__ @globals\/globals.h@
-}
anotherGlobalConstant_ptr :: Ptr.Ptr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8123602967122676

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_staticConst_ptr@
-}
foreign import ccall unsafe "hs_bindgen_d04ef3b41afccd4c" hs_bindgen_d04ef3b41afccd4c ::
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
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d04ef3b41afccd4c

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_classless_ptr@
-}
foreign import ccall unsafe "hs_bindgen_5d93bd707e83de07" hs_bindgen_5d93bd707e83de07 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE classless_ptr #-}

{-| No storage class specified

__C declaration:__ @classless@

__defined at:__ @globals\/globals.h:457:11@

__exported by:__ @globals\/globals.h@
-}
classless_ptr :: Ptr.Ptr FC.CInt
classless_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_5d93bd707e83de07

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_constArray1_ptr@
-}
foreign import ccall unsafe "hs_bindgen_7779e72b6ab72de1" hs_bindgen_7779e72b6ab72de1 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))

{-# NOINLINE constArray1_ptr #-}

{-| A an array of size 4 containing constant integers

__C declaration:__ @constArray1@

__defined at:__ @globals\/globals.h:460:18@

__exported by:__ @globals\/globals.h@
-}
constArray1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7779e72b6ab72de1

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constArray1_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_constArray2_ptr@
-}
foreign import ccall unsafe "hs_bindgen_fed813bd4083c3c5" hs_bindgen_fed813bd4083c3c5 ::
     IO (Ptr.Ptr ConstIntArray)

{-# NOINLINE constArray2_ptr #-}

{-| __C declaration:__ @constArray2@

    __defined at:__ @globals\/globals.h:464:22@

    __exported by:__ @globals\/globals.h@
-}
constArray2_ptr :: Ptr.Ptr ConstIntArray
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fed813bd4083c3c5

{-| __unique:__ @test_globalsglobals_Example_get_constTuple_ptr@
-}
foreign import ccall unsafe "hs_bindgen_07cd8b2ac394e36c" hs_bindgen_07cd8b2ac394e36c ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE constTuple_ptr #-}

{-| A constant tuple

__C declaration:__ @constTuple@

__defined at:__ @globals\/globals.h:468:27@

__exported by:__ @globals\/globals.h@
-}
constTuple_ptr :: Ptr.Ptr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_07cd8b2ac394e36c

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_nonConstTuple_ptr@
-}
foreign import ccall unsafe "hs_bindgen_d5ea9aa1a5cbcedb" hs_bindgen_d5ea9aa1a5cbcedb ::
     IO (Ptr.Ptr Tuple)

{-# NOINLINE nonConstTuple_ptr #-}

{-| A non-constant tuple with a constant member

__C declaration:__ @nonConstTuple@

__defined at:__ @globals\/globals.h:470:21@

__exported by:__ @globals\/globals.h@
-}
nonConstTuple_ptr :: Ptr.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_d5ea9aa1a5cbcedb

{-| __unique:__ @test_globalsglobals_Example_get_ptrToConstInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_f167ac4f659d037d" hs_bindgen_f167ac4f659d037d ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE ptrToConstInt_ptr #-}

{-| A pointer to const int

__C declaration:__ @ptrToConstInt@

__defined at:__ @globals\/globals.h:473:20@

__exported by:__ @globals\/globals.h@
-}
ptrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_f167ac4f659d037d

{-| __unique:__ @test_globalsglobals_Example_get_constPtrToInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_8376621614875e3d" hs_bindgen_8376621614875e3d ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToInt_ptr #-}

{-| A const pointer to int

__C declaration:__ @constPtrToInt@

__defined at:__ @globals\/globals.h:475:20@

__exported by:__ @globals\/globals.h@
-}
constPtrToInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_8376621614875e3d

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: Ptr.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

{-| __unique:__ @test_globalsglobals_Example_get_constPtrToConstInt_ptr@
-}
foreign import ccall unsafe "hs_bindgen_afb4f6b5c6ec422e" hs_bindgen_afb4f6b5c6ec422e ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToConstInt_ptr #-}

{-| A const pointer to const int

__C declaration:__ @constPtrToConstInt@

__defined at:__ @globals\/globals.h:477:26@

__exported by:__ @globals\/globals.h@
-}
constPtrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_afb4f6b5c6ec422e

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: Ptr.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
