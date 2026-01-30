{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.Internal.CAPI
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.LibC
import Example

$(HsBindgen.Runtime.Internal.CAPI.addCSource (HsBindgen.Runtime.Internal.CAPI.unlines
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
  , "/* test_globalsglobals_Example_get_anonPoint */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_6a74c096c69eaf13 (void)"
  , "{"
  , "  return &anonPoint;"
  , "}"
  , "/* test_globalsglobals_Example_get_anonPair */"
  , "__attribute__ ((const))"
  , "void *hs_bindgen_2ee54a93dbbed801 (void)"
  , "{"
  , "  return &anonPair;"
  , "}"
  , "/* test_globalsglobals_Example_get_anonEnum */"
  , "__attribute__ ((const))"
  , "unsigned int *hs_bindgen_1c28cee33967e767 (void)"
  , "{"
  , "  return &anonEnum;"
  , "}"
  , "/* test_globalsglobals_Example_get_anonEnumCoords */"
  , "__attribute__ ((const))"
  , "unsigned int *hs_bindgen_0e0ec9b1d935fbb6 (void)"
  , "{"
  , "  return &anonEnumCoords;"
  , "}"
  ]))

-- __unique:__ @test_globalsglobals_Example_get_simpleGlobal@
foreign import ccall unsafe "hs_bindgen_4f8e7b3d91414aa8" hs_bindgen_4f8e7b3d91414aa8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_simpleGlobal@
hs_bindgen_4f8e7b3d91414aa8 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_4f8e7b3d91414aa8 =
  RIP.fromFFIType hs_bindgen_4f8e7b3d91414aa8_base

{-# NOINLINE simpleGlobal #-}
{-| Global variables

__C declaration:__ @simpleGlobal@

__defined at:__ @globals\/globals.h 9:12@

__exported by:__ @globals\/globals.h@
-}
simpleGlobal :: RIP.Ptr RIP.CInt
simpleGlobal =
  RIP.unsafePerformIO hs_bindgen_4f8e7b3d91414aa8

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal1@
foreign import ccall unsafe "hs_bindgen_7f4cd619c55119dd" hs_bindgen_7f4cd619c55119dd_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal1@
hs_bindgen_7f4cd619c55119dd :: IO (RIP.Ptr Config)
hs_bindgen_7f4cd619c55119dd =
  RIP.fromFFIType hs_bindgen_7f4cd619c55119dd_base

{-# NOINLINE compoundGlobal1 #-}
{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h 16:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1 :: RIP.Ptr Config
compoundGlobal1 =
  RIP.unsafePerformIO hs_bindgen_7f4cd619c55119dd

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal2@
foreign import ccall unsafe "hs_bindgen_ed5c7196c3291592" hs_bindgen_ed5c7196c3291592_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal2@
hs_bindgen_ed5c7196c3291592 :: IO (RIP.Ptr Inline_struct)
hs_bindgen_ed5c7196c3291592 =
  RIP.fromFFIType hs_bindgen_ed5c7196c3291592_base

{-# NOINLINE compoundGlobal2 #-}
{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h 19:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2 :: RIP.Ptr Inline_struct
compoundGlobal2 =
  RIP.unsafePerformIO hs_bindgen_ed5c7196c3291592

-- __unique:__ @test_globalsglobals_Example_get_nesInteger@
foreign import ccall unsafe "hs_bindgen_0be07820afb78239" hs_bindgen_0be07820afb78239_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesInteger@
hs_bindgen_0be07820afb78239 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_0be07820afb78239 =
  RIP.fromFFIType hs_bindgen_0be07820afb78239_base

{-# NOINLINE nesInteger #-}
{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

__C declaration:__ @nesInteger@

__defined at:__ @globals\/globals.h 35:9@

__exported by:__ @globals\/globals.h@
-}
nesInteger :: RIP.Ptr RIP.CInt
nesInteger =
  RIP.unsafePerformIO hs_bindgen_0be07820afb78239

-- __unique:__ @test_globalsglobals_Example_get_nesFloating@
foreign import ccall unsafe "hs_bindgen_e3497c0a80a77750" hs_bindgen_e3497c0a80a77750_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesFloating@
hs_bindgen_e3497c0a80a77750 :: IO (RIP.Ptr RIP.CFloat)
hs_bindgen_e3497c0a80a77750 =
  RIP.fromFFIType hs_bindgen_e3497c0a80a77750_base

{-# NOINLINE nesFloating #-}
{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h 36:9@

    __exported by:__ @globals\/globals.h@
-}
nesFloating :: RIP.Ptr RIP.CFloat
nesFloating =
  RIP.unsafePerformIO hs_bindgen_e3497c0a80a77750

-- __unique:__ @test_globalsglobals_Example_get_nesString1@
foreign import ccall unsafe "hs_bindgen_78918168bc760476" hs_bindgen_78918168bc760476_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesString1@
hs_bindgen_78918168bc760476 :: IO (RIP.Ptr (RIP.Ptr RIP.CChar))
hs_bindgen_78918168bc760476 =
  RIP.fromFFIType hs_bindgen_78918168bc760476_base

{-# NOINLINE nesString1 #-}
{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h 38:9@

    __exported by:__ @globals\/globals.h@
-}
nesString1 :: RIP.Ptr (RIP.Ptr RIP.CChar)
nesString1 =
  RIP.unsafePerformIO hs_bindgen_78918168bc760476

-- __unique:__ @test_globalsglobals_Example_get_nesString2@
foreign import ccall unsafe "hs_bindgen_c6c52463f890e752" hs_bindgen_c6c52463f890e752_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesString2@
hs_bindgen_c6c52463f890e752 :: IO (RIP.Ptr ((CA.ConstantArray 3) RIP.CChar))
hs_bindgen_c6c52463f890e752 =
  RIP.fromFFIType hs_bindgen_c6c52463f890e752_base

{-# NOINLINE nesString2 #-}
{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h 39:9@

    __exported by:__ @globals\/globals.h@
-}
nesString2 :: RIP.Ptr ((CA.ConstantArray 3) RIP.CChar)
nesString2 =
  RIP.unsafePerformIO hs_bindgen_c6c52463f890e752

-- __unique:__ @test_globalsglobals_Example_get_nesCharacter@
foreign import ccall unsafe "hs_bindgen_9b33d990c25069a0" hs_bindgen_9b33d990c25069a0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCharacter@
hs_bindgen_9b33d990c25069a0 :: IO (RIP.Ptr RIP.CChar)
hs_bindgen_9b33d990c25069a0 =
  RIP.fromFFIType hs_bindgen_9b33d990c25069a0_base

{-# NOINLINE nesCharacter #-}
{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h 40:9@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter :: RIP.Ptr RIP.CChar
nesCharacter =
  RIP.unsafePerformIO hs_bindgen_9b33d990c25069a0

-- __unique:__ @test_globalsglobals_Example_get_nesParen@
foreign import ccall unsafe "hs_bindgen_561a1d5a05307329" hs_bindgen_561a1d5a05307329_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesParen@
hs_bindgen_561a1d5a05307329 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_561a1d5a05307329 =
  RIP.fromFFIType hs_bindgen_561a1d5a05307329_base

{-# NOINLINE nesParen #-}
{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h 41:9@

    __exported by:__ @globals\/globals.h@
-}
nesParen :: RIP.Ptr RIP.CInt
nesParen =
  RIP.unsafePerformIO hs_bindgen_561a1d5a05307329

-- __unique:__ @test_globalsglobals_Example_get_nesUnary@
foreign import ccall unsafe "hs_bindgen_4d3d64def4cf943f" hs_bindgen_4d3d64def4cf943f_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesUnary@
hs_bindgen_4d3d64def4cf943f :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_4d3d64def4cf943f =
  RIP.fromFFIType hs_bindgen_4d3d64def4cf943f_base

{-# NOINLINE nesUnary #-}
{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h 42:9@

    __exported by:__ @globals\/globals.h@
-}
nesUnary :: RIP.Ptr RIP.CInt
nesUnary =
  RIP.unsafePerformIO hs_bindgen_4d3d64def4cf943f

-- __unique:__ @test_globalsglobals_Example_get_nesBinary@
foreign import ccall unsafe "hs_bindgen_dcb8301e1cb444b7" hs_bindgen_dcb8301e1cb444b7_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesBinary@
hs_bindgen_dcb8301e1cb444b7 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_dcb8301e1cb444b7 =
  RIP.fromFFIType hs_bindgen_dcb8301e1cb444b7_base

{-# NOINLINE nesBinary #-}
{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h 43:9@

    __exported by:__ @globals\/globals.h@
-}
nesBinary :: RIP.Ptr RIP.CInt
nesBinary =
  RIP.unsafePerformIO hs_bindgen_dcb8301e1cb444b7

-- __unique:__ @test_globalsglobals_Example_get_nesConditional@
foreign import ccall unsafe "hs_bindgen_798d6b9c7136a5d0" hs_bindgen_798d6b9c7136a5d0_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesConditional@
hs_bindgen_798d6b9c7136a5d0 :: IO (RIP.Ptr RIP.CInt)
hs_bindgen_798d6b9c7136a5d0 =
  RIP.fromFFIType hs_bindgen_798d6b9c7136a5d0_base

{-# NOINLINE nesConditional #-}
{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h 44:9@

    __exported by:__ @globals\/globals.h@
-}
nesConditional :: RIP.Ptr RIP.CInt
nesConditional =
  RIP.unsafePerformIO hs_bindgen_798d6b9c7136a5d0

-- __unique:__ @test_globalsglobals_Example_get_nesCast@
foreign import ccall unsafe "hs_bindgen_9c15dc9805f8abb8" hs_bindgen_9c15dc9805f8abb8_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCast@
hs_bindgen_9c15dc9805f8abb8 :: IO (RIP.Ptr RIP.CFloat)
hs_bindgen_9c15dc9805f8abb8 =
  RIP.fromFFIType hs_bindgen_9c15dc9805f8abb8_base

{-# NOINLINE nesCast #-}
{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h 45:9@

    __exported by:__ @globals\/globals.h@
-}
nesCast :: RIP.Ptr RIP.CFloat
nesCast =
  RIP.unsafePerformIO hs_bindgen_9c15dc9805f8abb8

-- __unique:__ @test_globalsglobals_Example_get_nesCompound@
foreign import ccall unsafe "hs_bindgen_089dfddcc6667ac2" hs_bindgen_089dfddcc6667ac2_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCompound@
hs_bindgen_089dfddcc6667ac2 :: IO (RIP.Ptr (RIP.Ptr RIP.CInt))
hs_bindgen_089dfddcc6667ac2 =
  RIP.fromFFIType hs_bindgen_089dfddcc6667ac2_base

{-# NOINLINE nesCompound #-}
{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h 46:9@

    __exported by:__ @globals\/globals.h@
-}
nesCompound :: RIP.Ptr (RIP.Ptr RIP.CInt)
nesCompound =
  RIP.unsafePerformIO hs_bindgen_089dfddcc6667ac2

-- __unique:__ @test_globalsglobals_Example_get_nesInitList@
foreign import ccall unsafe "hs_bindgen_798af9a98bfc3030" hs_bindgen_798af9a98bfc3030_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesInitList@
hs_bindgen_798af9a98bfc3030 :: IO (RIP.Ptr ((CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8))
hs_bindgen_798af9a98bfc3030 =
  RIP.fromFFIType hs_bindgen_798af9a98bfc3030_base

{-# NOINLINE nesInitList #-}
{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h 47:9@

    __exported by:__ @globals\/globals.h@
-}
nesInitList :: RIP.Ptr ((CA.ConstantArray 4) HsBindgen.Runtime.LibC.Word8)
nesInitList =
  RIP.unsafePerformIO hs_bindgen_798af9a98bfc3030

-- __unique:__ @test_globalsglobals_Example_get_nesBool@
foreign import ccall unsafe "hs_bindgen_846b0fde4d102012" hs_bindgen_846b0fde4d102012_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesBool@
hs_bindgen_846b0fde4d102012 :: IO (RIP.Ptr RIP.CBool)
hs_bindgen_846b0fde4d102012 =
  RIP.fromFFIType hs_bindgen_846b0fde4d102012_base

{-# NOINLINE nesBool #-}
{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h 48:9@

    __exported by:__ @globals\/globals.h@
-}
nesBool :: RIP.Ptr RIP.CBool
nesBool =
  RIP.unsafePerformIO hs_bindgen_846b0fde4d102012

-- __unique:__ @test_globalsglobals_Example_get_streamBinary@
foreign import ccall unsafe "hs_bindgen_b243f9b292f8b883" hs_bindgen_b243f9b292f8b883_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_streamBinary@
hs_bindgen_b243f9b292f8b883 :: IO (RIP.Ptr ((CA.ConstantArray 4096) HsBindgen.Runtime.LibC.Word8))
hs_bindgen_b243f9b292f8b883 =
  RIP.fromFFIType hs_bindgen_b243f9b292f8b883_base

{-# NOINLINE streamBinary #-}
{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

__C declaration:__ @streamBinary@

__defined at:__ @globals\/globals.h 60:9@

__exported by:__ @globals\/globals.h@
-}
streamBinary :: RIP.Ptr ((CA.ConstantArray 4096) HsBindgen.Runtime.LibC.Word8)
streamBinary =
  RIP.unsafePerformIO hs_bindgen_b243f9b292f8b883

-- __unique:__ @test_globalsglobals_Example_get_streamBinary_len@
foreign import ccall unsafe "hs_bindgen_60adad2a6178e6cc" hs_bindgen_60adad2a6178e6cc_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_streamBinary_len@
hs_bindgen_60adad2a6178e6cc :: IO (RIP.Ptr HsBindgen.Runtime.LibC.Word32)
hs_bindgen_60adad2a6178e6cc =
  RIP.fromFFIType hs_bindgen_60adad2a6178e6cc_base

{-# NOINLINE streamBinary_len #-}
{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h 404:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len :: RIP.Ptr HsBindgen.Runtime.LibC.Word32
streamBinary_len =
  RIP.unsafePerformIO hs_bindgen_60adad2a6178e6cc

-- __unique:__ @test_globalsglobals_Example_get_some_global_struct@
foreign import ccall unsafe "hs_bindgen_fe50ca9a4fea641c" hs_bindgen_fe50ca9a4fea641c_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_some_global_struct@
hs_bindgen_fe50ca9a4fea641c :: IO (RIP.Ptr Struct2_t)
hs_bindgen_fe50ca9a4fea641c =
  RIP.fromFFIType hs_bindgen_fe50ca9a4fea641c_base

{-# NOINLINE some_global_struct #-}
{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h 425:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct :: RIP.Ptr Struct2_t
some_global_struct =
  RIP.unsafePerformIO hs_bindgen_fe50ca9a4fea641c

-- __unique:__ @test_globalsglobals_Example_get_anonPoint@
foreign import ccall unsafe "hs_bindgen_6a74c096c69eaf13" hs_bindgen_6a74c096c69eaf13_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_anonPoint@
hs_bindgen_6a74c096c69eaf13 :: IO (RIP.Ptr AnonPoint)
hs_bindgen_6a74c096c69eaf13 =
  RIP.fromFFIType hs_bindgen_6a74c096c69eaf13_base

{-# NOINLINE anonPoint #-}
{-| __C declaration:__ @anonPoint@

    __defined at:__ @globals\/globals.h 438:26@

    __exported by:__ @globals\/globals.h@
-}
anonPoint :: RIP.Ptr AnonPoint
anonPoint =
  RIP.unsafePerformIO hs_bindgen_6a74c096c69eaf13

-- __unique:__ @test_globalsglobals_Example_get_anonPair@
foreign import ccall unsafe "hs_bindgen_2ee54a93dbbed801" hs_bindgen_2ee54a93dbbed801_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_anonPair@
hs_bindgen_2ee54a93dbbed801 :: IO (RIP.Ptr AnonPair)
hs_bindgen_2ee54a93dbbed801 =
  RIP.fromFFIType hs_bindgen_2ee54a93dbbed801_base

{-# NOINLINE anonPair #-}
{-| __C declaration:__ @anonPair@

    __defined at:__ @globals\/globals.h 441:26@

    __exported by:__ @globals\/globals.h@
-}
anonPair :: RIP.Ptr AnonPair
anonPair =
  RIP.unsafePerformIO hs_bindgen_2ee54a93dbbed801

-- __unique:__ @test_globalsglobals_Example_get_anonEnum@
foreign import ccall unsafe "hs_bindgen_1c28cee33967e767" hs_bindgen_1c28cee33967e767_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_anonEnum@
hs_bindgen_1c28cee33967e767 :: IO (RIP.Ptr AnonEnum)
hs_bindgen_1c28cee33967e767 =
  RIP.fromFFIType hs_bindgen_1c28cee33967e767_base

{-# NOINLINE anonEnum #-}
{-| __C declaration:__ @anonEnum@

    __defined at:__ @globals\/globals.h 444:31@

    __exported by:__ @globals\/globals.h@
-}
anonEnum :: RIP.Ptr AnonEnum
anonEnum =
  RIP.unsafePerformIO hs_bindgen_1c28cee33967e767

-- __unique:__ @test_globalsglobals_Example_get_anonEnumCoords@
foreign import ccall unsafe "hs_bindgen_0e0ec9b1d935fbb6" hs_bindgen_0e0ec9b1d935fbb6_base ::
     IO (RIP.Ptr RIP.Void)

-- __unique:__ @test_globalsglobals_Example_get_anonEnumCoords@
hs_bindgen_0e0ec9b1d935fbb6 :: IO (RIP.Ptr AnonEnumCoords)
hs_bindgen_0e0ec9b1d935fbb6 =
  RIP.fromFFIType hs_bindgen_0e0ec9b1d935fbb6_base

{-# NOINLINE anonEnumCoords #-}
{-| __C declaration:__ @anonEnumCoords@

    __defined at:__ @globals\/globals.h 447:33@

    __exported by:__ @globals\/globals.h@
-}
anonEnumCoords :: RIP.Ptr AnonEnumCoords
anonEnumCoords =
  RIP.unsafePerformIO hs_bindgen_0e0ec9b1d935fbb6
