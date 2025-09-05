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
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <globals.h>\n/* get_simpleGlobal_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_9e13cdab849fd6a3 (void) { return &simpleGlobal; } \n/* get_compoundGlobal1_ptr */ __attribute__ ((const)) struct config *hs_bindgen_test_globals_9093ee3b5b63dbb9 (void) { return &compoundGlobal1; } \n/* get_compoundGlobal2_ptr */ __attribute__ ((const)) struct inline_struct *hs_bindgen_test_globals_35cfb530c6e3b540 (void) { return &compoundGlobal2; } \n/* get_nesInteger_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_d0e555bab6218b45 (void) { return &nesInteger; } \n/* get_nesFloating_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_620d3eeb41be6814 (void) { return &nesFloating; } \n/* get_nesString1_ptr */ __attribute__ ((const)) char **hs_bindgen_test_globals_58609a874bbd4939 (void) { return &nesString1; } \n/* get_nesString2_ptr */ __attribute__ ((const)) char (*hs_bindgen_test_globals_d24d15726a247083 (void))[3] { return &nesString2; } \n/* get_nesCharacter_ptr */ __attribute__ ((const)) char *hs_bindgen_test_globals_472e8cff06767166 (void) { return &nesCharacter; } \n/* get_nesParen_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3535fbeb41ad5a41 (void) { return &nesParen; } \n/* get_nesUnary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_c2e1dc65064ad658 (void) { return &nesUnary; } \n/* get_nesBinary_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_3d0448526008a072 (void) { return &nesBinary; } \n/* get_nesConditional_ptr */ __attribute__ ((const)) signed int *hs_bindgen_test_globals_6733c2e7c59bc620 (void) { return &nesConditional; } \n/* get_nesCast_ptr */ __attribute__ ((const)) float *hs_bindgen_test_globals_d6e6e72f287d9b41 (void) { return &nesCast; } \n/* get_nesCompound_ptr */ __attribute__ ((const)) signed int **hs_bindgen_test_globals_032905c6b7a5e39f (void) { return &nesCompound; } \n/* get_nesInitList_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_4012de1fec3423a7 (void))[4] { return &nesInitList; } \n/* get_nesBool_ptr */ __attribute__ ((const)) _Bool *hs_bindgen_test_globals_f9fb23513d064767 (void) { return &nesBool; } \n/* get_streamBinary_ptr */ __attribute__ ((const)) uint8_t (*hs_bindgen_test_globals_92e68af3ae2ed3fb (void))[4096] { return &streamBinary; } \n/* get_streamBinary_len_ptr */ __attribute__ ((const)) uint32_t *hs_bindgen_test_globals_8d6f9f3043208163 (void) { return &streamBinary_len; } \n/* get_some_global_struct_ptr */ __attribute__ ((const)) struct2_t *hs_bindgen_test_globals_88ad1f87a451c285 (void) { return &some_global_struct; } \n/* get_globalConstant_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2875ba0f7feba4fd (void) { return &globalConstant; } \n/* get_anotherGlobalConstant_ptr */ __attribute__ ((const)) ConstInt *hs_bindgen_test_globals_6ebecf881bce1334 (void) { return &anotherGlobalConstant; } \n/* get_staticConst_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_2eea936ed4beec74 (void) { return &staticConst; } \n/* get_classless_ptr */ __attribute__ ((const)) signed int const *hs_bindgen_test_globals_5d631acbb16c0e7e (void) { return &classless; } \n/* get_constArray1_ptr */ __attribute__ ((const)) signed int const (*hs_bindgen_test_globals_0d7a9340f4ef8b2e (void))[4] { return &constArray1; } \n/* get_constArray2_ptr */ __attribute__ ((const)) ConstIntArray *hs_bindgen_test_globals_7e09340985caec8d (void) { return &constArray2; } \n/* get_constTuple_ptr */ __attribute__ ((const)) struct tuple const *hs_bindgen_test_globals_6f2e1968e15f0b9b (void) { return &constTuple; } \n/* get_nonConstTuple_ptr */ __attribute__ ((const)) struct tuple *hs_bindgen_test_globals_e8e62512a4e5d162 (void) { return &nonConstTuple; } \n/* get_ptrToConstInt_ptr */ __attribute__ ((const)) signed int const **hs_bindgen_test_globals_e41146f6df20fe0d (void) { return &ptrToConstInt; } \n/* get_constPtrToInt_ptr */ __attribute__ ((const)) signed int *const *hs_bindgen_test_globals_83b0d0f5488b6a03 (void) { return &constPtrToInt; } \n/* get_constPtrToConstInt_ptr */ __attribute__ ((const)) signed int const *const *hs_bindgen_test_globals_247c0c91ce28a18d (void) { return &constPtrToConstInt; } \n")

{-| Global variables

  __from C:__ @simpleGlobal@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_9e13cdab849fd6a3" hs_bindgen_test_globals_9e13cdab849fd6a3
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE simpleGlobal_ptr #-}

simpleGlobal_ptr :: F.Ptr FC.CInt
simpleGlobal_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_9e13cdab849fd6a3

data Config = Config
  { config_x :: FC.CInt
  , config_y :: FC.CInt
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

foreign import ccall unsafe "hs_bindgen_test_globals_9093ee3b5b63dbb9" hs_bindgen_test_globals_9093ee3b5b63dbb9
  :: IO (F.Ptr Config)

{-# NOINLINE compoundGlobal1_ptr #-}

compoundGlobal1_ptr :: F.Ptr Config
compoundGlobal1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_9093ee3b5b63dbb9

data Inline_struct = Inline_struct
  { inline_struct_x :: FC.CInt
  , inline_struct_y :: FC.CInt
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

foreign import ccall unsafe "hs_bindgen_test_globals_35cfb530c6e3b540" hs_bindgen_test_globals_35cfb530c6e3b540
  :: IO (F.Ptr Inline_struct)

{-# NOINLINE compoundGlobal2_ptr #-}

compoundGlobal2_ptr :: F.Ptr Inline_struct
compoundGlobal2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_35cfb530c6e3b540

{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

  __from C:__ @nesInteger@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_d0e555bab6218b45" hs_bindgen_test_globals_d0e555bab6218b45
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE nesInteger_ptr #-}

nesInteger_ptr :: F.Ptr FC.CInt
nesInteger_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d0e555bab6218b45

foreign import ccall unsafe "hs_bindgen_test_globals_620d3eeb41be6814" hs_bindgen_test_globals_620d3eeb41be6814
  :: IO (F.Ptr FC.CFloat)

{-# NOINLINE nesFloating_ptr #-}

nesFloating_ptr :: F.Ptr FC.CFloat
nesFloating_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_620d3eeb41be6814

foreign import ccall unsafe "hs_bindgen_test_globals_58609a874bbd4939" hs_bindgen_test_globals_58609a874bbd4939
  :: IO (F.Ptr (F.Ptr FC.CChar))

{-# NOINLINE nesString1_ptr #-}

nesString1_ptr :: F.Ptr (F.Ptr FC.CChar)
nesString1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_58609a874bbd4939

foreign import ccall unsafe "hs_bindgen_test_globals_d24d15726a247083" hs_bindgen_test_globals_d24d15726a247083
  :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar))

{-# NOINLINE nesString2_ptr #-}

nesString2_ptr :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)
nesString2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d24d15726a247083

foreign import ccall unsafe "hs_bindgen_test_globals_472e8cff06767166" hs_bindgen_test_globals_472e8cff06767166
  :: IO (F.Ptr FC.CChar)

{-# NOINLINE nesCharacter_ptr #-}

nesCharacter_ptr :: F.Ptr FC.CChar
nesCharacter_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_472e8cff06767166

foreign import ccall unsafe "hs_bindgen_test_globals_3535fbeb41ad5a41" hs_bindgen_test_globals_3535fbeb41ad5a41
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE nesParen_ptr #-}

nesParen_ptr :: F.Ptr FC.CInt
nesParen_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_3535fbeb41ad5a41

foreign import ccall unsafe "hs_bindgen_test_globals_c2e1dc65064ad658" hs_bindgen_test_globals_c2e1dc65064ad658
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE nesUnary_ptr #-}

nesUnary_ptr :: F.Ptr FC.CInt
nesUnary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_c2e1dc65064ad658

foreign import ccall unsafe "hs_bindgen_test_globals_3d0448526008a072" hs_bindgen_test_globals_3d0448526008a072
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE nesBinary_ptr #-}

nesBinary_ptr :: F.Ptr FC.CInt
nesBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_3d0448526008a072

foreign import ccall unsafe "hs_bindgen_test_globals_6733c2e7c59bc620" hs_bindgen_test_globals_6733c2e7c59bc620
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE nesConditional_ptr #-}

nesConditional_ptr :: F.Ptr FC.CInt
nesConditional_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6733c2e7c59bc620

foreign import ccall unsafe "hs_bindgen_test_globals_d6e6e72f287d9b41" hs_bindgen_test_globals_d6e6e72f287d9b41
  :: IO (F.Ptr FC.CFloat)

{-# NOINLINE nesCast_ptr #-}

nesCast_ptr :: F.Ptr FC.CFloat
nesCast_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_d6e6e72f287d9b41

foreign import ccall unsafe "hs_bindgen_test_globals_032905c6b7a5e39f" hs_bindgen_test_globals_032905c6b7a5e39f
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE nesCompound_ptr #-}

nesCompound_ptr :: F.Ptr (F.Ptr FC.CInt)
nesCompound_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_032905c6b7a5e39f

foreign import ccall unsafe "hs_bindgen_test_globals_4012de1fec3423a7" hs_bindgen_test_globals_4012de1fec3423a7
  :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE nesInitList_ptr #-}

nesInitList_ptr :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)
nesInitList_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_4012de1fec3423a7

foreign import ccall unsafe "hs_bindgen_test_globals_f9fb23513d064767" hs_bindgen_test_globals_f9fb23513d064767
  :: IO (F.Ptr FC.CBool)

{-# NOINLINE nesBool_ptr #-}

nesBool_ptr :: F.Ptr FC.CBool
nesBool_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_f9fb23513d064767

{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

  __from C:__ @streamBinary@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_92e68af3ae2ed3fb" hs_bindgen_test_globals_92e68af3ae2ed3fb
  :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8))

{-# NOINLINE streamBinary_ptr #-}

streamBinary_ptr :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)
streamBinary_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_92e68af3ae2ed3fb

foreign import ccall unsafe "hs_bindgen_test_globals_8d6f9f3043208163" hs_bindgen_test_globals_8d6f9f3043208163
  :: IO (F.Ptr HsBindgen.Runtime.Prelude.Word32)

{-# NOINLINE streamBinary_len_ptr #-}

streamBinary_len_ptr :: F.Ptr HsBindgen.Runtime.Prelude.Word32
streamBinary_len_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_8d6f9f3043208163

data Version_t = Version_t
  { version_t_major :: HsBindgen.Runtime.Prelude.Word8
  , version_t_minor :: HsBindgen.Runtime.Prelude.Word16
  , version_t_patch :: HsBindgen.Runtime.Prelude.Word8
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

data Struct1_t = Struct1_t
  { struct1_t_x :: HsBindgen.Runtime.Prelude.Word16
  , struct1_t_y :: FC.CBool
  , struct1_t_version :: Version_t
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

data Struct2_t = Struct2_t
  { struct2_t_field1 :: Struct1_t
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

foreign import ccall unsafe "hs_bindgen_test_globals_88ad1f87a451c285" hs_bindgen_test_globals_88ad1f87a451c285
  :: IO (F.Ptr Struct2_t)

{-# NOINLINE some_global_struct_ptr #-}

some_global_struct_ptr :: F.Ptr Struct2_t
some_global_struct_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_88ad1f87a451c285

{-| Constant

  Although this is a constant, we don't expect an initializer (since it's `extern`).

  __from C:__ @globalConstant@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_2875ba0f7feba4fd" hs_bindgen_test_globals_2875ba0f7feba4fd
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE globalConstant_ptr #-}

globalConstant_ptr :: F.Ptr FC.CInt
globalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_2875ba0f7feba4fd

{-# NOINLINE globalConstant #-}

globalConstant :: FC.CInt
globalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek globalConstant_ptr)

{-| Constant, through typedef

  __from C:__ @ConstInt@
-}
newtype ConstInt = ConstInt
  { un_ConstInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

foreign import ccall unsafe "hs_bindgen_test_globals_6ebecf881bce1334" hs_bindgen_test_globals_6ebecf881bce1334
  :: IO (F.Ptr ConstInt)

{-# NOINLINE anotherGlobalConstant_ptr #-}

anotherGlobalConstant_ptr :: F.Ptr ConstInt
anotherGlobalConstant_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6ebecf881bce1334

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

{-| Constant, but local to the file

  Unlike with `extern`, in this we _do_ expect an initializer.

  __from C:__ @staticConst@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_2eea936ed4beec74" hs_bindgen_test_globals_2eea936ed4beec74
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE staticConst_ptr #-}

staticConst_ptr :: F.Ptr FC.CInt
staticConst_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_2eea936ed4beec74

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

{-| No storage class specified

  __from C:__ @classless@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_5d631acbb16c0e7e" hs_bindgen_test_globals_5d631acbb16c0e7e
  :: IO (F.Ptr FC.CInt)

{-# NOINLINE classless_ptr #-}

classless_ptr :: F.Ptr FC.CInt
classless_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_5d631acbb16c0e7e

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

{-| A an array of size 4 containing constant integers

  __from C:__ @constArray1@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_0d7a9340f4ef8b2e" hs_bindgen_test_globals_0d7a9340f4ef8b2e
  :: IO (F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt))

{-# NOINLINE constArray1_ptr #-}

constArray1_ptr :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)
constArray1_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_0d7a9340f4ef8b2e

{-# NOINLINE constArray1 #-}

constArray1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt
constArray1 =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constArray1_ptr)

{-| An array of uknown size containing constant integers

  __from C:__ @ConstIntArray@
-}
newtype ConstIntArray = ConstIntArray
  { un_ConstIntArray :: HsBindgen.Runtime.IncompleteArray.IncompleteArray FC.CInt
  }
  deriving stock (Eq, Show)

foreign import ccall unsafe "hs_bindgen_test_globals_7e09340985caec8d" hs_bindgen_test_globals_7e09340985caec8d
  :: IO (F.Ptr ConstIntArray)

{-# NOINLINE constArray2_ptr #-}

constArray2_ptr :: F.Ptr ConstIntArray
constArray2_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_7e09340985caec8d

data Tuple = Tuple
  { tuple_x :: FC.CInt
  , tuple_y :: FC.CInt
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

{-| A constant tuple

  __from C:__ @constTuple@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_6f2e1968e15f0b9b" hs_bindgen_test_globals_6f2e1968e15f0b9b
  :: IO (F.Ptr Tuple)

{-# NOINLINE constTuple_ptr #-}

constTuple_ptr :: F.Ptr Tuple
constTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_6f2e1968e15f0b9b

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

{-| A non-constant tuple with a constant member

  __from C:__ @nonConstTuple@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_e8e62512a4e5d162" hs_bindgen_test_globals_e8e62512a4e5d162
  :: IO (F.Ptr Tuple)

{-# NOINLINE nonConstTuple_ptr #-}

nonConstTuple_ptr :: F.Ptr Tuple
nonConstTuple_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_e8e62512a4e5d162

{-| A pointer to const int

  __from C:__ @ptrToConstInt@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_e41146f6df20fe0d" hs_bindgen_test_globals_e41146f6df20fe0d
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE ptrToConstInt_ptr #-}

ptrToConstInt_ptr :: F.Ptr (F.Ptr FC.CInt)
ptrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_e41146f6df20fe0d

{-| A const pointer to int

  __from C:__ @constPtrToInt@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_83b0d0f5488b6a03" hs_bindgen_test_globals_83b0d0f5488b6a03
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE constPtrToInt_ptr #-}

constPtrToInt_ptr :: F.Ptr (F.Ptr FC.CInt)
constPtrToInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_83b0d0f5488b6a03

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: F.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

{-| A const pointer to const int

  __from C:__ @constPtrToConstInt@
-}
foreign import ccall unsafe "hs_bindgen_test_globals_247c0c91ce28a18d" hs_bindgen_test_globals_247c0c91ce28a18d
  :: IO (F.Ptr (F.Ptr FC.CInt))

{-# NOINLINE constPtrToConstInt_ptr #-}

constPtrToConstInt_ptr :: F.Ptr (F.Ptr FC.CInt)
constPtrToConstInt_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_globals_247c0c91ce28a18d

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: F.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
