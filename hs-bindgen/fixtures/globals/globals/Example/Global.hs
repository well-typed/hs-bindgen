{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.Prelude
import Example
import Prelude (IO)

$(HsBindgen.Runtime.Prelude.addCSource (HsBindgen.Runtime.Prelude.unlines
  [ "#include <globals/globals.h>"
  , "/* test_globalsglobals_Example_get_simpleGlobal */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_4f8e7b3d91414aa8 (void)"
  , "{"
  , "  return &simpleGlobal;"
  , "}"
  , "/* test_globalsglobals_Example_get_compoundGlobal1 */"
  , "__attribute__ ((const))"
  , "struct config *hs_bindgen_7f4cd619c55119dd (void)"
  , "{"
  , "  return &compoundGlobal1;"
  , "}"
  , "/* test_globalsglobals_Example_get_compoundGlobal2 */"
  , "__attribute__ ((const))"
  , "struct inline_struct *hs_bindgen_ed5c7196c3291592 (void)"
  , "{"
  , "  return &compoundGlobal2;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesInteger */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_0be07820afb78239 (void)"
  , "{"
  , "  return &nesInteger;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesFloating */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_e3497c0a80a77750 (void)"
  , "{"
  , "  return &nesFloating;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesString1 */"
  , "__attribute__ ((const))"
  , "char **hs_bindgen_78918168bc760476 (void)"
  , "{"
  , "  return &nesString1;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesString2 */"
  , "__attribute__ ((const))"
  , "char (*hs_bindgen_c6c52463f890e752 (void))[3]"
  , "{"
  , "  return &nesString2;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCharacter */"
  , "__attribute__ ((const))"
  , "char *hs_bindgen_9b33d990c25069a0 (void)"
  , "{"
  , "  return &nesCharacter;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesParen */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_561a1d5a05307329 (void)"
  , "{"
  , "  return &nesParen;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesUnary */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_4d3d64def4cf943f (void)"
  , "{"
  , "  return &nesUnary;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesBinary */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_dcb8301e1cb444b7 (void)"
  , "{"
  , "  return &nesBinary;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesConditional */"
  , "__attribute__ ((const))"
  , "signed int *hs_bindgen_798d6b9c7136a5d0 (void)"
  , "{"
  , "  return &nesConditional;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCast */"
  , "__attribute__ ((const))"
  , "float *hs_bindgen_9c15dc9805f8abb8 (void)"
  , "{"
  , "  return &nesCast;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesCompound */"
  , "__attribute__ ((const))"
  , "signed int **hs_bindgen_089dfddcc6667ac2 (void)"
  , "{"
  , "  return &nesCompound;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesInitList */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_798af9a98bfc3030 (void))[4]"
  , "{"
  , "  return &nesInitList;"
  , "}"
  , "/* test_globalsglobals_Example_get_nesBool */"
  , "__attribute__ ((const))"
  , "_Bool *hs_bindgen_846b0fde4d102012 (void)"
  , "{"
  , "  return &nesBool;"
  , "}"
  , "/* test_globalsglobals_Example_get_streamBinary */"
  , "__attribute__ ((const))"
  , "uint8_t (*hs_bindgen_b243f9b292f8b883 (void))[4096]"
  , "{"
  , "  return &streamBinary;"
  , "}"
  , "/* test_globalsglobals_Example_get_streamBinary_len */"
  , "__attribute__ ((const))"
  , "uint32_t *hs_bindgen_60adad2a6178e6cc (void)"
  , "{"
  , "  return &streamBinary_len;"
  , "}"
  , "/* test_globalsglobals_Example_get_some_global_struct */"
  , "__attribute__ ((const))"
  , "struct2_t *hs_bindgen_fe50ca9a4fea641c (void)"
  , "{"
  , "  return &some_global_struct;"
  , "}"
  ]))

-- | __unique:__ @test_globalsglobals_Example_get_simpleGlobal@
foreign import ccall unsafe "hs_bindgen_4f8e7b3d91414aa8" hs_bindgen_4f8e7b3d91414aa8 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE simpleGlobal #-}

{-| Global variables

__C declaration:__ @simpleGlobal@

__defined at:__ @globals\/globals.h:9:12@

__exported by:__ @globals\/globals.h@
-}
simpleGlobal :: Ptr.Ptr FC.CInt
simpleGlobal =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4f8e7b3d91414aa8

-- | __unique:__ @test_globalsglobals_Example_get_compoundGlobal1@
foreign import ccall unsafe "hs_bindgen_7f4cd619c55119dd" hs_bindgen_7f4cd619c55119dd ::
     IO (Ptr.Ptr Config)

{-# NOINLINE compoundGlobal1 #-}

{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h:16:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1 :: Ptr.Ptr Config
compoundGlobal1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_7f4cd619c55119dd

-- | __unique:__ @test_globalsglobals_Example_get_compoundGlobal2@
foreign import ccall unsafe "hs_bindgen_ed5c7196c3291592" hs_bindgen_ed5c7196c3291592 ::
     IO (Ptr.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2 #-}

{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h:19:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2 :: Ptr.Ptr Inline_struct
compoundGlobal2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_ed5c7196c3291592

-- | __unique:__ @test_globalsglobals_Example_get_nesInteger@
foreign import ccall unsafe "hs_bindgen_0be07820afb78239" hs_bindgen_0be07820afb78239 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesInteger #-}

{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

__C declaration:__ @nesInteger@

__defined at:__ @globals\/globals.h:35:9@

__exported by:__ @globals\/globals.h@
-}
nesInteger :: Ptr.Ptr FC.CInt
nesInteger =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_0be07820afb78239

-- | __unique:__ @test_globalsglobals_Example_get_nesFloating@
foreign import ccall unsafe "hs_bindgen_e3497c0a80a77750" hs_bindgen_e3497c0a80a77750 ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesFloating #-}

{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h:36:9@

    __exported by:__ @globals\/globals.h@
-}
nesFloating :: Ptr.Ptr FC.CFloat
nesFloating =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_e3497c0a80a77750

-- | __unique:__ @test_globalsglobals_Example_get_nesString1@
foreign import ccall unsafe "hs_bindgen_78918168bc760476" hs_bindgen_78918168bc760476 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE nesString1 #-}

{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h:38:9@

    __exported by:__ @globals\/globals.h@
-}
nesString1 :: Ptr.Ptr (Ptr.Ptr FC.CChar)
nesString1 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_78918168bc760476

-- | __unique:__ @test_globalsglobals_Example_get_nesString2@
foreign import ccall unsafe "hs_bindgen_c6c52463f890e752" hs_bindgen_c6c52463f890e752 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2 #-}

{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h:39:9@

    __exported by:__ @globals\/globals.h@
-}
nesString2 :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2 =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_c6c52463f890e752

-- | __unique:__ @test_globalsglobals_Example_get_nesCharacter@
foreign import ccall unsafe "hs_bindgen_9b33d990c25069a0" hs_bindgen_9b33d990c25069a0 ::
     IO (Ptr.Ptr FC.CChar)

{-# NOINLINE nesCharacter #-}

{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h:40:9@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter :: Ptr.Ptr FC.CChar
nesCharacter =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9b33d990c25069a0

-- | __unique:__ @test_globalsglobals_Example_get_nesParen@
foreign import ccall unsafe "hs_bindgen_561a1d5a05307329" hs_bindgen_561a1d5a05307329 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesParen #-}

{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h:41:9@

    __exported by:__ @globals\/globals.h@
-}
nesParen :: Ptr.Ptr FC.CInt
nesParen =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_561a1d5a05307329

-- | __unique:__ @test_globalsglobals_Example_get_nesUnary@
foreign import ccall unsafe "hs_bindgen_4d3d64def4cf943f" hs_bindgen_4d3d64def4cf943f ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesUnary #-}

{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h:42:9@

    __exported by:__ @globals\/globals.h@
-}
nesUnary :: Ptr.Ptr FC.CInt
nesUnary =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_4d3d64def4cf943f

-- | __unique:__ @test_globalsglobals_Example_get_nesBinary@
foreign import ccall unsafe "hs_bindgen_dcb8301e1cb444b7" hs_bindgen_dcb8301e1cb444b7 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesBinary #-}

{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h:43:9@

    __exported by:__ @globals\/globals.h@
-}
nesBinary :: Ptr.Ptr FC.CInt
nesBinary =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_dcb8301e1cb444b7

-- | __unique:__ @test_globalsglobals_Example_get_nesConditional@
foreign import ccall unsafe "hs_bindgen_798d6b9c7136a5d0" hs_bindgen_798d6b9c7136a5d0 ::
     IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesConditional #-}

{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h:44:9@

    __exported by:__ @globals\/globals.h@
-}
nesConditional :: Ptr.Ptr FC.CInt
nesConditional =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_798d6b9c7136a5d0

-- | __unique:__ @test_globalsglobals_Example_get_nesCast@
foreign import ccall unsafe "hs_bindgen_9c15dc9805f8abb8" hs_bindgen_9c15dc9805f8abb8 ::
     IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesCast #-}

{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h:45:9@

    __exported by:__ @globals\/globals.h@
-}
nesCast :: Ptr.Ptr FC.CFloat
nesCast =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_9c15dc9805f8abb8

-- | __unique:__ @test_globalsglobals_Example_get_nesCompound@
foreign import ccall unsafe "hs_bindgen_089dfddcc6667ac2" hs_bindgen_089dfddcc6667ac2 ::
     IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE nesCompound #-}

{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h:46:9@

    __exported by:__ @globals\/globals.h@
-}
nesCompound :: Ptr.Ptr (Ptr.Ptr FC.CInt)
nesCompound =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_089dfddcc6667ac2

-- | __unique:__ @test_globalsglobals_Example_get_nesInitList@
foreign import ccall unsafe "hs_bindgen_798af9a98bfc3030" hs_bindgen_798af9a98bfc3030 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList #-}

{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h:47:9@

    __exported by:__ @globals\/globals.h@
-}
nesInitList :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_798af9a98bfc3030

-- | __unique:__ @test_globalsglobals_Example_get_nesBool@
foreign import ccall unsafe "hs_bindgen_846b0fde4d102012" hs_bindgen_846b0fde4d102012 ::
     IO (Ptr.Ptr FC.CBool)

{-# NOINLINE nesBool #-}

{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h:48:9@

    __exported by:__ @globals\/globals.h@
-}
nesBool :: Ptr.Ptr FC.CBool
nesBool =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_846b0fde4d102012

-- | __unique:__ @test_globalsglobals_Example_get_streamBinary@
foreign import ccall unsafe "hs_bindgen_b243f9b292f8b883" hs_bindgen_b243f9b292f8b883 ::
     IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE streamBinary #-}

{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

__C declaration:__ @streamBinary@

__defined at:__ @globals\/globals.h:60:9@

__exported by:__ @globals\/globals.h@
-}
streamBinary :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)
streamBinary =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_b243f9b292f8b883

-- | __unique:__ @test_globalsglobals_Example_get_streamBinary_len@
foreign import ccall unsafe "hs_bindgen_60adad2a6178e6cc" hs_bindgen_60adad2a6178e6cc ::
     IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len #-}

{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h:404:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_60adad2a6178e6cc

-- | __unique:__ @test_globalsglobals_Example_get_some_global_struct@
foreign import ccall unsafe "hs_bindgen_fe50ca9a4fea641c" hs_bindgen_fe50ca9a4fea641c ::
     IO (Ptr.Ptr Struct2_t)

{-# NOINLINE some_global_struct #-}

{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h:425:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct :: Ptr.Ptr Struct2_t
some_global_struct =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_fe50ca9a4fea641c
