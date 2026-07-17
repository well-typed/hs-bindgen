{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK prune #-}

module Example.Global
    ( Example.Global.simpleGlobal
    , Example.Global.compoundGlobal1
    , Example.Global.compoundGlobal2
    , Example.Global.nesInteger
    , Example.Global.nesFloating
    , Example.Global.nesImaginary
    , Example.Global.nesString1
    , Example.Global.nesString2
    , Example.Global.nesCharacter
    , Example.Global.nesParen
    , Example.Global.nesUnary
    , Example.Global.nesBinary
    , Example.Global.nesConditional
    , Example.Global.nesCast
    , Example.Global.nesCompound
    , Example.Global.nesInitList
    , Example.Global.nesBool
    , Example.Global.streamBinary
    , Example.Global.streamBinary_len
    , Example.Global.some_global_struct
    )
  where

import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CAPI
import Example

$(HsBindgen.Runtime.Support.CAPI.addCSource (HsBindgen.Runtime.Support.CAPI.unlines
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
  , "/* test_globalsglobals_Example_get_nesImaginary */"
  , "__attribute__ ((const))"
  , "double _Complex *hs_bindgen_e958634c96270349 (void)"
  , "{"
  , "  return &nesImaginary;"
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

-- __unique:__ @test_globalsglobals_Example_get_simpleGlobal@
foreign import ccall unsafe "hs_bindgen_4f8e7b3d91414aa8" hs_bindgen_4f8e7b3d91414aa8_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_simpleGlobal@
hs_bindgen_4f8e7b3d91414aa8 :: IO (BG.Ptr BG.CInt)
hs_bindgen_4f8e7b3d91414aa8 =
  BG.fromFFIType hs_bindgen_4f8e7b3d91414aa8_base

{-# NOINLINE simpleGlobal #-}
{-| __C declaration:__ @simpleGlobal@

    __defined at:__ @globals\/globals.h 10:12@

    __exported by:__ @globals\/globals.h@
-}
simpleGlobal :: BG.Ptr BG.CInt
simpleGlobal =
  BG.unsafePerformIO hs_bindgen_4f8e7b3d91414aa8

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal1@
foreign import ccall unsafe "hs_bindgen_7f4cd619c55119dd" hs_bindgen_7f4cd619c55119dd_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal1@
hs_bindgen_7f4cd619c55119dd :: IO (BG.Ptr Config)
hs_bindgen_7f4cd619c55119dd =
  BG.fromFFIType hs_bindgen_7f4cd619c55119dd_base

{-# NOINLINE compoundGlobal1 #-}
{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals\/globals.h 17:22@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal1 :: BG.Ptr Config
compoundGlobal1 =
  BG.unsafePerformIO hs_bindgen_7f4cd619c55119dd

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal2@
foreign import ccall unsafe "hs_bindgen_ed5c7196c3291592" hs_bindgen_ed5c7196c3291592_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_compoundGlobal2@
hs_bindgen_ed5c7196c3291592 :: IO (BG.Ptr Inline_struct)
hs_bindgen_ed5c7196c3291592 =
  BG.fromFFIType hs_bindgen_ed5c7196c3291592_base

{-# NOINLINE compoundGlobal2 #-}
{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals\/globals.h 20:47@

    __exported by:__ @globals\/globals.h@
-}
compoundGlobal2 :: BG.Ptr Inline_struct
compoundGlobal2 =
  BG.unsafePerformIO hs_bindgen_ed5c7196c3291592

-- __unique:__ @test_globalsglobals_Example_get_nesInteger@
foreign import ccall unsafe "hs_bindgen_0be07820afb78239" hs_bindgen_0be07820afb78239_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesInteger@
hs_bindgen_0be07820afb78239 :: IO (BG.Ptr BG.CInt)
hs_bindgen_0be07820afb78239 =
  BG.fromFFIType hs_bindgen_0be07820afb78239_base

{-# NOINLINE nesInteger #-}
{-| Non-extern non-static global variables

    These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

    We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of @CXCursor@ [1](https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013) , starting at @CXCursor_IntegerLiteral@ ; see also definition of 'varDecl' in @HsBindgen.Frontend.Pass.Parse.Decl@ .

    __C declaration:__ @nesInteger@

    __defined at:__ @globals\/globals.h 36:16@

    __exported by:__ @globals\/globals.h@
-}
nesInteger :: BG.Ptr BG.CInt
nesInteger =
  BG.unsafePerformIO hs_bindgen_0be07820afb78239

-- __unique:__ @test_globalsglobals_Example_get_nesFloating@
foreign import ccall unsafe "hs_bindgen_e3497c0a80a77750" hs_bindgen_e3497c0a80a77750_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesFloating@
hs_bindgen_e3497c0a80a77750 :: IO (BG.Ptr BG.CFloat)
hs_bindgen_e3497c0a80a77750 =
  BG.fromFFIType hs_bindgen_e3497c0a80a77750_base

{-# NOINLINE nesFloating #-}
{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals\/globals.h 37:16@

    __exported by:__ @globals\/globals.h@
-}
nesFloating :: BG.Ptr BG.CFloat
nesFloating =
  BG.unsafePerformIO hs_bindgen_e3497c0a80a77750

-- __unique:__ @test_globalsglobals_Example_get_nesImaginary@
foreign import ccall unsafe "hs_bindgen_e958634c96270349" hs_bindgen_e958634c96270349_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesImaginary@
hs_bindgen_e958634c96270349 :: IO (BG.Ptr (BG.Complex BG.CDouble))
hs_bindgen_e958634c96270349 =
  BG.fromFFIType hs_bindgen_e958634c96270349_base

{-# NOINLINE nesImaginary #-}
{-| __C declaration:__ @nesImaginary@

    __defined at:__ @globals\/globals.h 38:16@

    __exported by:__ @globals\/globals.h@
-}
nesImaginary :: BG.Ptr (BG.Complex BG.CDouble)
nesImaginary =
  BG.unsafePerformIO hs_bindgen_e958634c96270349

-- __unique:__ @test_globalsglobals_Example_get_nesString1@
foreign import ccall unsafe "hs_bindgen_78918168bc760476" hs_bindgen_78918168bc760476_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesString1@
hs_bindgen_78918168bc760476 :: IO (BG.Ptr (BG.Ptr BG.CChar))
hs_bindgen_78918168bc760476 =
  BG.fromFFIType hs_bindgen_78918168bc760476_base

{-# NOINLINE nesString1 #-}
{-| __C declaration:__ @nesString1@

    __defined at:__ @globals\/globals.h 39:16@

    __exported by:__ @globals\/globals.h@
-}
nesString1 :: BG.Ptr (BG.Ptr BG.CChar)
nesString1 =
  BG.unsafePerformIO hs_bindgen_78918168bc760476

-- __unique:__ @test_globalsglobals_Example_get_nesString2@
foreign import ccall unsafe "hs_bindgen_c6c52463f890e752" hs_bindgen_c6c52463f890e752_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesString2@
hs_bindgen_c6c52463f890e752 :: IO (BG.Ptr (CA.ConstantArray 3 BG.CChar))
hs_bindgen_c6c52463f890e752 =
  BG.fromFFIType hs_bindgen_c6c52463f890e752_base

{-# NOINLINE nesString2 #-}
{-| __C declaration:__ @nesString2@

    __defined at:__ @globals\/globals.h 40:16@

    __exported by:__ @globals\/globals.h@
-}
nesString2 :: BG.Ptr (CA.ConstantArray 3 BG.CChar)
nesString2 =
  BG.unsafePerformIO hs_bindgen_c6c52463f890e752

-- __unique:__ @test_globalsglobals_Example_get_nesCharacter@
foreign import ccall unsafe "hs_bindgen_9b33d990c25069a0" hs_bindgen_9b33d990c25069a0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCharacter@
hs_bindgen_9b33d990c25069a0 :: IO (BG.Ptr BG.CChar)
hs_bindgen_9b33d990c25069a0 =
  BG.fromFFIType hs_bindgen_9b33d990c25069a0_base

{-# NOINLINE nesCharacter #-}
{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals\/globals.h 41:16@

    __exported by:__ @globals\/globals.h@
-}
nesCharacter :: BG.Ptr BG.CChar
nesCharacter =
  BG.unsafePerformIO hs_bindgen_9b33d990c25069a0

-- __unique:__ @test_globalsglobals_Example_get_nesParen@
foreign import ccall unsafe "hs_bindgen_561a1d5a05307329" hs_bindgen_561a1d5a05307329_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesParen@
hs_bindgen_561a1d5a05307329 :: IO (BG.Ptr BG.CInt)
hs_bindgen_561a1d5a05307329 =
  BG.fromFFIType hs_bindgen_561a1d5a05307329_base

{-# NOINLINE nesParen #-}
{-| __C declaration:__ @nesParen@

    __defined at:__ @globals\/globals.h 42:16@

    __exported by:__ @globals\/globals.h@
-}
nesParen :: BG.Ptr BG.CInt
nesParen =
  BG.unsafePerformIO hs_bindgen_561a1d5a05307329

-- __unique:__ @test_globalsglobals_Example_get_nesUnary@
foreign import ccall unsafe "hs_bindgen_4d3d64def4cf943f" hs_bindgen_4d3d64def4cf943f_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesUnary@
hs_bindgen_4d3d64def4cf943f :: IO (BG.Ptr BG.CInt)
hs_bindgen_4d3d64def4cf943f =
  BG.fromFFIType hs_bindgen_4d3d64def4cf943f_base

{-# NOINLINE nesUnary #-}
{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals\/globals.h 43:16@

    __exported by:__ @globals\/globals.h@
-}
nesUnary :: BG.Ptr BG.CInt
nesUnary =
  BG.unsafePerformIO hs_bindgen_4d3d64def4cf943f

-- __unique:__ @test_globalsglobals_Example_get_nesBinary@
foreign import ccall unsafe "hs_bindgen_dcb8301e1cb444b7" hs_bindgen_dcb8301e1cb444b7_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesBinary@
hs_bindgen_dcb8301e1cb444b7 :: IO (BG.Ptr BG.CInt)
hs_bindgen_dcb8301e1cb444b7 =
  BG.fromFFIType hs_bindgen_dcb8301e1cb444b7_base

{-# NOINLINE nesBinary #-}
{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals\/globals.h 44:16@

    __exported by:__ @globals\/globals.h@
-}
nesBinary :: BG.Ptr BG.CInt
nesBinary =
  BG.unsafePerformIO hs_bindgen_dcb8301e1cb444b7

-- __unique:__ @test_globalsglobals_Example_get_nesConditional@
foreign import ccall unsafe "hs_bindgen_798d6b9c7136a5d0" hs_bindgen_798d6b9c7136a5d0_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesConditional@
hs_bindgen_798d6b9c7136a5d0 :: IO (BG.Ptr BG.CInt)
hs_bindgen_798d6b9c7136a5d0 =
  BG.fromFFIType hs_bindgen_798d6b9c7136a5d0_base

{-# NOINLINE nesConditional #-}
{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals\/globals.h 45:16@

    __exported by:__ @globals\/globals.h@
-}
nesConditional :: BG.Ptr BG.CInt
nesConditional =
  BG.unsafePerformIO hs_bindgen_798d6b9c7136a5d0

-- __unique:__ @test_globalsglobals_Example_get_nesCast@
foreign import ccall unsafe "hs_bindgen_9c15dc9805f8abb8" hs_bindgen_9c15dc9805f8abb8_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCast@
hs_bindgen_9c15dc9805f8abb8 :: IO (BG.Ptr BG.CFloat)
hs_bindgen_9c15dc9805f8abb8 =
  BG.fromFFIType hs_bindgen_9c15dc9805f8abb8_base

{-# NOINLINE nesCast #-}
{-| __C declaration:__ @nesCast@

    __defined at:__ @globals\/globals.h 46:16@

    __exported by:__ @globals\/globals.h@
-}
nesCast :: BG.Ptr BG.CFloat
nesCast =
  BG.unsafePerformIO hs_bindgen_9c15dc9805f8abb8

-- __unique:__ @test_globalsglobals_Example_get_nesCompound@
foreign import ccall unsafe "hs_bindgen_089dfddcc6667ac2" hs_bindgen_089dfddcc6667ac2_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesCompound@
hs_bindgen_089dfddcc6667ac2 :: IO (BG.Ptr (BG.Ptr BG.CInt))
hs_bindgen_089dfddcc6667ac2 =
  BG.fromFFIType hs_bindgen_089dfddcc6667ac2_base

{-# NOINLINE nesCompound #-}
{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals\/globals.h 47:16@

    __exported by:__ @globals\/globals.h@
-}
nesCompound :: BG.Ptr (BG.Ptr BG.CInt)
nesCompound =
  BG.unsafePerformIO hs_bindgen_089dfddcc6667ac2

-- __unique:__ @test_globalsglobals_Example_get_nesInitList@
foreign import ccall unsafe "hs_bindgen_798af9a98bfc3030" hs_bindgen_798af9a98bfc3030_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesInitList@
hs_bindgen_798af9a98bfc3030 :: IO (BG.Ptr (CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8))
hs_bindgen_798af9a98bfc3030 =
  BG.fromFFIType hs_bindgen_798af9a98bfc3030_base

{-# NOINLINE nesInitList #-}
{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals\/globals.h 48:16@

    __exported by:__ @globals\/globals.h@
-}
nesInitList :: BG.Ptr (CA.ConstantArray 4 HsBindgen.Runtime.LibC.Word8)
nesInitList =
  BG.unsafePerformIO hs_bindgen_798af9a98bfc3030

-- __unique:__ @test_globalsglobals_Example_get_nesBool@
foreign import ccall unsafe "hs_bindgen_846b0fde4d102012" hs_bindgen_846b0fde4d102012_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_nesBool@
hs_bindgen_846b0fde4d102012 :: IO (BG.Ptr BG.CBool)
hs_bindgen_846b0fde4d102012 =
  BG.fromFFIType hs_bindgen_846b0fde4d102012_base

{-# NOINLINE nesBool #-}
{-| __C declaration:__ @nesBool@

    __defined at:__ @globals\/globals.h 49:16@

    __exported by:__ @globals\/globals.h@
-}
nesBool :: BG.Ptr BG.CBool
nesBool =
  BG.unsafePerformIO hs_bindgen_846b0fde4d102012

-- __unique:__ @test_globalsglobals_Example_get_streamBinary@
foreign import ccall unsafe "hs_bindgen_b243f9b292f8b883" hs_bindgen_b243f9b292f8b883_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_streamBinary@
hs_bindgen_b243f9b292f8b883 :: IO (BG.Ptr (CA.ConstantArray 4096 HsBindgen.Runtime.LibC.Word8))
hs_bindgen_b243f9b292f8b883 =
  BG.fromFFIType hs_bindgen_b243f9b292f8b883_base

{-# NOINLINE streamBinary #-}
{-| Additional examples of global variables, abstracted from real examples

    The @'streamBinary'@ / @'streamBinary_len'@ example comes from [1](https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325) , and is an example of a non-extern non-static global (indeed, the header does not even use @pragma once@ or similar).

    __C declaration:__ @streamBinary@

    __defined at:__ @globals\/globals.h 61:9@

    __exported by:__ @globals\/globals.h@
-}
streamBinary :: BG.Ptr (CA.ConstantArray 4096 HsBindgen.Runtime.LibC.Word8)
streamBinary =
  BG.unsafePerformIO hs_bindgen_b243f9b292f8b883

-- __unique:__ @test_globalsglobals_Example_get_streamBinary_len@
foreign import ccall unsafe "hs_bindgen_60adad2a6178e6cc" hs_bindgen_60adad2a6178e6cc_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_streamBinary_len@
hs_bindgen_60adad2a6178e6cc :: IO (BG.Ptr HsBindgen.Runtime.LibC.Word32)
hs_bindgen_60adad2a6178e6cc =
  BG.fromFFIType hs_bindgen_60adad2a6178e6cc_base

{-# NOINLINE streamBinary_len #-}
{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals\/globals.h 405:10@

    __exported by:__ @globals\/globals.h@
-}
streamBinary_len :: BG.Ptr HsBindgen.Runtime.LibC.Word32
streamBinary_len =
  BG.unsafePerformIO hs_bindgen_60adad2a6178e6cc

-- __unique:__ @test_globalsglobals_Example_get_some_global_struct@
foreign import ccall unsafe "hs_bindgen_fe50ca9a4fea641c" hs_bindgen_fe50ca9a4fea641c_base ::
     IO (BG.Ptr BG.Void)

-- __unique:__ @test_globalsglobals_Example_get_some_global_struct@
hs_bindgen_fe50ca9a4fea641c :: IO (BG.Ptr Struct2_t)
hs_bindgen_fe50ca9a4fea641c =
  BG.fromFFIType hs_bindgen_fe50ca9a4fea641c_base

{-# NOINLINE some_global_struct #-}
{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals\/globals.h 426:11@

    __exported by:__ @globals\/globals.h@
-}
some_global_struct :: BG.Ptr Struct2_t
some_global_struct =
  BG.unsafePerformIO hs_bindgen_fe50ca9a4fea641c
