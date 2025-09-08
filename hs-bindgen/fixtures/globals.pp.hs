{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as Ptr
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <globals.h>\n/* get_simpleGlobal_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_9e13cdab849fd6a3 (void) { return &simpleGlobal; } \n/* get_compoundGlobal1_ptr */ __attribute__ ((const)) struct config *hs_bindgen_test_globals_9093ee3b5b63dbb9 (void) { return &compoundGlobal1; } \n/* get_compoundGlobal2_ptr */ __attribute__ ((const)) struct inline_struct *hs_bindgen_test_globals_35cfb530c6e3b540 (void) { return &compoundGlobal2; } \n/* get_nesInteger_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_d0e555bab6218b45 (void) { return &nesInteger; } \n/* get_nesFloating_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_620d3eeb41be6814 (void) { return &nesFloating; } \n/* get_nesString1_ptr */ __attribute__ ((const)) char **hs_bindgen_test_globals_58609a874bbd4939 (void) { return &nesString1; } \n/* get_nesString2_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_globals_d24d15726a247083 (void))[3] { return &nesString2; } \n/* get_nesCharacter_ptr */ __attribute__ ((const)) char *hs_bindgen_test_globals_472e8cff06767166 (void) { return &nesCharacter; } \n/* get_nesParen_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3535fbeb41ad5a41 (void) { return &nesParen; } \n/* get_nesUnary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_c2e1dc65064ad658 (void) { return &nesUnary; } \n/* get_nesBinary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3d0448526008a072 (void) { return &nesBinary; } \n/* get_nesConditional_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_6733c2e7c59bc620 (void) { return &nesConditional; } \n/* get_nesCast_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_d6e6e72f287d9b41 (void) { return &nesCast; } \n/* get_nesCompound_ptr */ __attribute__ ((const)) signed int **hs_bindgen_test_globals_032905c6b7a5e39f (void) { return &nesCompound; } \n/* get_nesInitList_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_4012de1fec3423a7 (void))[4] { return &nesInitList; } \n/* get_nesBool_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_globals_f9fb23513d064767 (void) { return &nesBool; } \n/* get_streamBinary_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_92e68af3ae2ed3fb (void))[4096] { return &streamBinary; } \n/* get_streamBinary_len_ptr */ __attribute__ ((const)) uint32_t *hs_bindgen_test_globals_8d6f9f3043208163 (void) { return &streamBinary_len; } \n/* get_some_global_struct_ptr */ __attribute__ ((const)) struct2_t *hs_bindgen_test_globals_88ad1f87a451c285 (void) { return &some_global_struct; } \n/* get_globalConstant_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2875ba0f7feba4fd (void) { return &globalConstant; } \n/* get_anotherGlobalConstant_ptr */ __attribute__ ((const)) ConstInt *hs_bindgen_test_globals_6ebecf881bce1334 (void) { return &anotherGlobalConstant; } \n/* get_staticConst_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2eea936ed4beec74 (void) { return &staticConst; } \n/* get_classless_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_5d631acbb16c0e7e (void) { return &classless; } \n/* get_constArray1_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_globals_0d7a9340f4ef8b2e (void))[4] { return &constArray1; } \n/* get_constArray2_ptr */ __attribute__ ((const)) ConstIntArray *hs_bindgen_test_globals_7e09340985caec8d (void) { return &constArray2; } \n/* get_constTuple_ptr */ __attribute__ ((const)) struct tuple const *hs_bindgen_test_globals_6f2e1968e15f0b9b (void) { return &constTuple; } \n/* get_nonConstTuple_ptr */ __attribute__ ((const)) struct tuple *hs_bindgen_test_globals_e8e62512a4e5d162 (void) { return &nonConstTuple; } \n/* get_ptrToConstInt_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_globals_e41146f6df20fe0d (void) { return &ptrToConstInt; } \n/* get_constPtrToInt_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_globals_83b0d0f5488b6a03 (void) { return &constPtrToInt; } \n/* get_constPtrToConstInt_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_globals_247c0c91ce28a18d (void) { return &constPtrToConstInt; } \n")

foreign import ccall unsafe "hs_bindgen_test_globals_9e13cdab849fd6a3" hs_bindgen_test_globals_9e13cdab849fd6a3
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE simpleGlobal_ptr #-}

simpleGlobal_ptr :: Ptr.Ptr FC.CInt
simpleGlobal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_9e13cdab849fd6a3

{-| __C declaration:__ @config@

    __defined at:__ @globals.h:12:8@

    __exported by:__ @globals.h@
-}
data Config = Config
  { config_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:13:7@

         __exported by:__ @globals.h@
    -}
  , config_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:14:7@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Config where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Config
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Config config_x2 config_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) config_x2
            >> F.pokeByteOff ptr0 (4 :: Int) config_y3

{-| __C declaration:__ @compoundGlobal1@

    __defined at:__ @globals.h:16:22@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_9093ee3b5b63dbb9" hs_bindgen_test_globals_9093ee3b5b63dbb9
  :: IO (Ptr.Ptr Config)

{-# NOINLINE compoundGlobal1_ptr #-}

compoundGlobal1_ptr :: Ptr.Ptr Config
compoundGlobal1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_9093ee3b5b63dbb9

{-| __C declaration:__ @inline_struct@

    __defined at:__ @globals.h:19:15@

    __exported by:__ @globals.h@
-}
data Inline_struct = Inline_struct
  { inline_struct_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:19:35@

         __exported by:__ @globals.h@
    -}
  , inline_struct_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:19:42@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Inline_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Inline_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Inline_struct inline_struct_x2 inline_struct_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) inline_struct_x2
            >> F.pokeByteOff ptr0 (4 :: Int) inline_struct_y3

{-| __C declaration:__ @compoundGlobal2@

    __defined at:__ @globals.h:19:47@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_35cfb530c6e3b540" hs_bindgen_test_globals_35cfb530c6e3b540
  :: IO (Ptr.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2_ptr #-}

compoundGlobal2_ptr :: Ptr.Ptr Inline_struct
compoundGlobal2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_35cfb530c6e3b540

{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

__C declaration:__ @nesInteger@

__defined at:__ @globals.h:35:9@

__exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_d0e555bab6218b45" hs_bindgen_test_globals_d0e555bab6218b45
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesInteger_ptr #-}

nesInteger_ptr :: Ptr.Ptr FC.CInt
nesInteger_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d0e555bab6218b45

{-| __C declaration:__ @nesFloating@

    __defined at:__ @globals.h:36:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_620d3eeb41be6814" hs_bindgen_test_globals_620d3eeb41be6814
  :: IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesFloating_ptr #-}

nesFloating_ptr :: Ptr.Ptr FC.CFloat
nesFloating_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_620d3eeb41be6814

{-| __C declaration:__ @nesString1@

    __defined at:__ @globals.h:38:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_58609a874bbd4939" hs_bindgen_test_globals_58609a874bbd4939
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CChar))

{-# NOINLINE nesString1_ptr #-}

nesString1_ptr :: Ptr.Ptr (Ptr.Ptr FC.CChar)
nesString1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_58609a874bbd4939

{-| __C declaration:__ @nesString2@

    __defined at:__ @globals.h:39:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_d24d15726a247083" hs_bindgen_test_globals_d24d15726a247083
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2_ptr #-}

nesString2_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d24d15726a247083

{-| __C declaration:__ @nesCharacter@

    __defined at:__ @globals.h:40:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_472e8cff06767166" hs_bindgen_test_globals_472e8cff06767166
  :: IO (Ptr.Ptr FC.CChar)

{-# NOINLINE nesCharacter_ptr #-}

nesCharacter_ptr :: Ptr.Ptr FC.CChar
nesCharacter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_472e8cff06767166

{-| __C declaration:__ @nesParen@

    __defined at:__ @globals.h:41:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_3535fbeb41ad5a41" hs_bindgen_test_globals_3535fbeb41ad5a41
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesParen_ptr #-}

nesParen_ptr :: Ptr.Ptr FC.CInt
nesParen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_3535fbeb41ad5a41

{-| __C declaration:__ @nesUnary@

    __defined at:__ @globals.h:42:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_c2e1dc65064ad658" hs_bindgen_test_globals_c2e1dc65064ad658
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesUnary_ptr #-}

nesUnary_ptr :: Ptr.Ptr FC.CInt
nesUnary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_c2e1dc65064ad658

{-| __C declaration:__ @nesBinary@

    __defined at:__ @globals.h:43:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_3d0448526008a072" hs_bindgen_test_globals_3d0448526008a072
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesBinary_ptr #-}

nesBinary_ptr :: Ptr.Ptr FC.CInt
nesBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_3d0448526008a072

{-| __C declaration:__ @nesConditional@

    __defined at:__ @globals.h:44:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_6733c2e7c59bc620" hs_bindgen_test_globals_6733c2e7c59bc620
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE nesConditional_ptr #-}

nesConditional_ptr :: Ptr.Ptr FC.CInt
nesConditional_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6733c2e7c59bc620

{-| __C declaration:__ @nesCast@

    __defined at:__ @globals.h:45:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_d6e6e72f287d9b41" hs_bindgen_test_globals_d6e6e72f287d9b41
  :: IO (Ptr.Ptr FC.CFloat)

{-# NOINLINE nesCast_ptr #-}

nesCast_ptr :: Ptr.Ptr FC.CFloat
nesCast_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d6e6e72f287d9b41

{-| __C declaration:__ @nesCompound@

    __defined at:__ @globals.h:46:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_032905c6b7a5e39f" hs_bindgen_test_globals_032905c6b7a5e39f
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE nesCompound_ptr #-}

nesCompound_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
nesCompound_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_032905c6b7a5e39f

{-| __C declaration:__ @nesInitList@

    __defined at:__ @globals.h:47:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_4012de1fec3423a7" hs_bindgen_test_globals_4012de1fec3423a7
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList_ptr #-}

nesInitList_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_4012de1fec3423a7

{-| __C declaration:__ @nesBool@

    __defined at:__ @globals.h:48:9@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_f9fb23513d064767" hs_bindgen_test_globals_f9fb23513d064767
  :: IO (Ptr.Ptr FC.CBool)

{-# NOINLINE nesBool_ptr #-}

nesBool_ptr :: Ptr.Ptr FC.CBool
nesBool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_f9fb23513d064767

{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

__C declaration:__ @streamBinary@

__defined at:__ @globals.h:60:9@

__exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_92e68af3ae2ed3fb" hs_bindgen_test_globals_92e68af3ae2ed3fb
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE streamBinary_ptr #-}

streamBinary_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)
streamBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_92e68af3ae2ed3fb

{-| __C declaration:__ @streamBinary_len@

    __defined at:__ @globals.h:404:10@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_8d6f9f3043208163" hs_bindgen_test_globals_8d6f9f3043208163
  :: IO (Ptr.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len_ptr #-}

streamBinary_len_ptr :: Ptr.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_8d6f9f3043208163

{-| __C declaration:__ @version_t@

    __defined at:__ @globals.h:406:9@

    __exported by:__ @globals.h@
-}
data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.Prelude.Word8
    {- ^ __C declaration:__ @major@

         __defined at:__ @globals.h:408:12@

         __exported by:__ @globals.h@
    -}
  , version_t_minor :: HsBindgen.Runtime.Prelude.Word16
    {- ^ __C declaration:__ @minor@

         __defined at:__ @globals.h:409:12@

         __exported by:__ @globals.h@
    -}
  , version_t_patch :: HsBindgen.Runtime.Prelude.Word8
    {- ^ __C declaration:__ @patch@

         __defined at:__ @globals.h:410:12@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Version_t where

  sizeOf = \_ -> (6 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Version_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Version_t version_t_major2 version_t_minor3 version_t_patch4 ->
               F.pokeByteOff ptr0 (0 :: Int) version_t_major2
            >> F.pokeByteOff ptr0 (2 :: Int) version_t_minor3
            >> F.pokeByteOff ptr0 (4 :: Int) version_t_patch4

{-| __C declaration:__ @struct1_t@

    __defined at:__ @globals.h:413:9@

    __exported by:__ @globals.h@
-}
data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.Prelude.Word16
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:415:13@

         __exported by:__ @globals.h@
    -}
  , struct1_t_y :: FC.CBool
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:416:13@

         __exported by:__ @globals.h@
    -}
  , struct1_t_version :: Version_t
    {- ^ __C declaration:__ @version@

         __defined at:__ @globals.h:417:13@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct1_t where

  sizeOf = \_ -> (10 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Struct1_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct1_t struct1_t_x2 struct1_t_y3 struct1_t_version4 ->
               F.pokeByteOff ptr0 (0 :: Int) struct1_t_x2
            >> F.pokeByteOff ptr0 (2 :: Int) struct1_t_y3
            >> F.pokeByteOff ptr0 (4 :: Int) struct1_t_version4

{-| __C declaration:__ @struct2_t@

    __defined at:__ @globals.h:420:9@

    __exported by:__ @globals.h@
-}
data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
    {- ^ __C declaration:__ @field1@

         __defined at:__ @globals.h:422:13@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Struct2_t where

  sizeOf = \_ -> (10 :: Int)

  alignment = \_ -> (2 :: Int)

  peek =
    \ptr0 ->
          pure Struct2_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Struct2_t struct2_t_field12 ->
            F.pokeByteOff ptr0 (0 :: Int) struct2_t_field12

{-| __C declaration:__ @some_global_struct@

    __defined at:__ @globals.h:425:11@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_88ad1f87a451c285" hs_bindgen_test_globals_88ad1f87a451c285
  :: IO (Ptr.Ptr Struct2_t)

{-# NOINLINE some_global_struct_ptr #-}

some_global_struct_ptr :: Ptr.Ptr Struct2_t
some_global_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_88ad1f87a451c285

{-| Constant

  Although this is a constant, we don't expect an initializer (since it's `extern`).

__C declaration:__ @globalConstant@

__defined at:__ @globals.h:445:18@

__exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_2875ba0f7feba4fd" hs_bindgen_test_globals_2875ba0f7feba4fd
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE globalConstant_ptr #-}

globalConstant_ptr :: Ptr.Ptr FC.CInt
globalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_2875ba0f7feba4fd

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek globalConstant_ptr)

newtype ConstInt = ConstInt
  { un_ConstInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

{-| __C declaration:__ @anotherGlobalConstant@

    __defined at:__ @globals.h:449:17@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_6ebecf881bce1334" hs_bindgen_test_globals_6ebecf881bce1334
  :: IO (Ptr.Ptr ConstInt)

{-# NOINLINE anotherGlobalConstant_ptr #-}

anotherGlobalConstant_ptr :: Ptr.Ptr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6ebecf881bce1334

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

{-| Constant, but local to the file

  Unlike with `extern`, in this we _do_ expect an initializer.

__C declaration:__ @staticConst@

__defined at:__ @globals.h:454:18@

__exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_2eea936ed4beec74" hs_bindgen_test_globals_2eea936ed4beec74
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE staticConst_ptr #-}

staticConst_ptr :: Ptr.Ptr FC.CInt
staticConst_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_2eea936ed4beec74

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

foreign import ccall unsafe "hs_bindgen_test_globals_5d631acbb16c0e7e" hs_bindgen_test_globals_5d631acbb16c0e7e
  :: IO (Ptr.Ptr FC.CInt)

{-# NOINLINE classless_ptr #-}

classless_ptr :: Ptr.Ptr FC.CInt
classless_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_5d631acbb16c0e7e

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

foreign import ccall unsafe "hs_bindgen_test_globals_0d7a9340f4ef8b2e" hs_bindgen_test_globals_0d7a9340f4ef8b2e
  :: IO (Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))

{-# NOINLINE constArray1_ptr #-}

constArray1_ptr :: Ptr.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_0d7a9340f4ef8b2e

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constArray1_ptr)

newtype ConstIntArray = ConstIntArray
  { un_ConstIntArray :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

{-| __C declaration:__ @constArray2@

    __defined at:__ @globals.h:464:22@

    __exported by:__ @globals.h@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_7e09340985caec8d" hs_bindgen_test_globals_7e09340985caec8d
  :: IO (Ptr.Ptr ConstIntArray)

{-# NOINLINE constArray2_ptr #-}

constArray2_ptr :: Ptr.Ptr ConstIntArray
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_7e09340985caec8d

{-| __C declaration:__ @tuple@

    __defined at:__ @globals.h:466:8@

    __exported by:__ @globals.h@
-}
data Tuple = Tuple
  { tuple_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @globals.h:466:20@

         __exported by:__ @globals.h@
    -}
  , tuple_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @globals.h:466:33@

         __exported by:__ @globals.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Tuple where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Tuple
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tuple tuple_x2 tuple_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) tuple_x2
            >> F.pokeByteOff ptr0 (4 :: Int) tuple_y3

foreign import ccall unsafe "hs_bindgen_test_globals_6f2e1968e15f0b9b" hs_bindgen_test_globals_6f2e1968e15f0b9b
  :: IO (Ptr.Ptr Tuple)

{-# NOINLINE constTuple_ptr #-}

constTuple_ptr :: Ptr.Ptr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6f2e1968e15f0b9b

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

foreign import ccall unsafe "hs_bindgen_test_globals_e8e62512a4e5d162" hs_bindgen_test_globals_e8e62512a4e5d162
  :: IO (Ptr.Ptr Tuple)

{-# NOINLINE nonConstTuple_ptr #-}

nonConstTuple_ptr :: Ptr.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_e8e62512a4e5d162

foreign import ccall unsafe "hs_bindgen_test_globals_e41146f6df20fe0d" hs_bindgen_test_globals_e41146f6df20fe0d
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE ptrToConstInt_ptr #-}

ptrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_e41146f6df20fe0d

foreign import ccall unsafe "hs_bindgen_test_globals_83b0d0f5488b6a03" hs_bindgen_test_globals_83b0d0f5488b6a03
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToInt_ptr #-}

constPtrToInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_83b0d0f5488b6a03

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: Ptr.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

foreign import ccall unsafe "hs_bindgen_test_globals_247c0c91ce28a18d" hs_bindgen_test_globals_247c0c91ce28a18d
  :: IO (Ptr.Ptr (Ptr.Ptr FC.CInt))

{-# NOINLINE constPtrToConstInt_ptr #-}

constPtrToConstInt_ptr :: Ptr.Ptr (Ptr.Ptr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_247c0c91ce28a18d

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: Ptr.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
