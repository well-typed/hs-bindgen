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
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

$(CAPI.addCSource "#include <globals.h>\n__attribute__ ((const)) signed int *get_simpleGlobal_ptr (void) { return &simpleGlobal; } \n__attribute__ ((const)) struct config *get_compoundGlobal1_ptr (void) { return &compoundGlobal1; } \n__attribute__ ((const)) struct inline_struct *get_compoundGlobal2_ptr (void) { return &compoundGlobal2; } \n__attribute__ ((const)) signed int *get_nesInteger_ptr (void) { return &nesInteger; } \n__attribute__ ((const)) float *get_nesFloating_ptr (void) { return &nesFloating; } \n__attribute__ ((const)) char **get_nesString1_ptr (void) { return &nesString1; } \n__attribute__ ((const)) char (*get_nesString2_ptr (void))[3] { return &nesString2; } \n__attribute__ ((const)) char *get_nesCharacter_ptr (void) { return &nesCharacter; } \n__attribute__ ((const)) signed int *get_nesParen_ptr (void) { return &nesParen; } \n__attribute__ ((const)) signed int *get_nesUnary_ptr (void) { return &nesUnary; } \n__attribute__ ((const)) signed int *get_nesBinary_ptr (void) { return &nesBinary; } \n__attribute__ ((const)) signed int *get_nesConditional_ptr (void) { return &nesConditional; } \n__attribute__ ((const)) float *get_nesCast_ptr (void) { return &nesCast; } \n__attribute__ ((const)) signed int **get_nesCompound_ptr (void) { return &nesCompound; } \n__attribute__ ((const)) uint8_t (*get_nesInitList_ptr (void))[4] { return &nesInitList; } \n__attribute__ ((const)) _Bool *get_nesBool_ptr (void) { return &nesBool; } \n__attribute__ ((const)) uint8_t (*get_streamBinary_ptr (void))[4096] { return &streamBinary; } \n__attribute__ ((const)) uint32_t *get_streamBinary_len_ptr (void) { return &streamBinary_len; } \n__attribute__ ((const)) struct2_t *get_some_global_struct_ptr (void) { return &some_global_struct; } \n__attribute__ ((const)) const signed int *get_globalConstant_ptr (void) { return &globalConstant; } \n__attribute__ ((const)) const ConstInt *get_anotherGlobalConstant_ptr (void) { return &anotherGlobalConstant; } \n__attribute__ ((const)) const signed int *get_staticConst_ptr (void) { return &staticConst; } \n__attribute__ ((const)) const signed int *get_classless_ptr (void) { return &classless; } \n__attribute__ ((const)) const signed int (*get_constArray1_ptr (void))[4] { return &constArray1; } \n__attribute__ ((const)) const ConstIntArray *get_constArray2_ptr (void) { return &constArray2; } \n__attribute__ ((const)) const struct tuple *get_constTuple_ptr (void) { return &constTuple; } \n__attribute__ ((const)) struct tuple *get_nonConstTuple_ptr (void) { return &nonConstTuple; } \n__attribute__ ((const)) signed int **get_ptrToConstInt_ptr (void) { return &ptrToConstInt; } \n__attribute__ ((const)) const signed int **get_constPtrToInt_ptr (void) { return &constPtrToInt; } \n__attribute__ ((const)) const signed int **get_constPtrToConstInt_ptr (void) { return &constPtrToConstInt; } \n")

{-| Global variables

  __from C:__ @simpleGlobal@
-}
foreign import ccall safe "get_simpleGlobal_ptr" simpleGlobal_ptr
  :: F.Ptr FC.CInt

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

foreign import ccall safe "get_compoundGlobal1_ptr" compoundGlobal1_ptr
  :: F.Ptr Config

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

foreign import ccall safe "get_compoundGlobal2_ptr" compoundGlobal2_ptr
  :: F.Ptr Inline_struct

{-| Non-extern non-static global variables

  These kinds of variables need to be treated with care, to avoid duplicate symbols, but do exist in the wild.

  We test with various kinds of initializers as we must explicitly ignore them in our parser. The list here roughly follows the definition of `CXCursor` [1], starting at `CXCursor_IntegerLiteral`; see also definition of 'varDecl' in `HsBindgen.Frontend.Pass.Parse.Decl`.

  [1]: https://clang.llvm.org/doxygen/group__CINDEX.html#gaaccc432245b4cd9f2d470913f9ef0013

  __from C:__ @nesInteger@
-}
foreign import ccall safe "get_nesInteger_ptr" nesInteger_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "get_nesFloating_ptr" nesFloating_ptr
  :: F.Ptr FC.CFloat

foreign import ccall safe "get_nesString1_ptr" nesString1_ptr
  :: F.Ptr (F.Ptr FC.CChar)

foreign import ccall safe "get_nesString2_ptr" nesString2_ptr
  :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CChar)

foreign import ccall safe "get_nesCharacter_ptr" nesCharacter_ptr
  :: F.Ptr FC.CChar

foreign import ccall safe "get_nesParen_ptr" nesParen_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "get_nesUnary_ptr" nesUnary_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "get_nesBinary_ptr" nesBinary_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "get_nesConditional_ptr" nesConditional_ptr
  :: F.Ptr FC.CInt

foreign import ccall safe "get_nesCast_ptr" nesCast_ptr
  :: F.Ptr FC.CFloat

foreign import ccall safe "get_nesCompound_ptr" nesCompound_ptr
  :: F.Ptr (F.Ptr FC.CInt)

foreign import ccall safe "get_nesInitList_ptr" nesInitList_ptr
  :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) HsBindgen.Runtime.Prelude.Word8)

foreign import ccall safe "get_nesBool_ptr" nesBool_ptr
  :: F.Ptr FC.CBool

{-| Additional examples of global variables, abstracted from real examples

  The `streamBinary`/`streamBinary_len` example comes from [1], and is an example of a non-extern non-static global (indeed, the header does not even use  once @ or similar).

  [1]: https://github.com/analogdevicesinc/no-OS/blob/855c4b3c34f2297865e448661ba4fcc0931bf430/drivers/rf-transceiver/talise/firmware/talise_stream_binary.h#L322-L325

  __from C:__ @streamBinary@
-}
foreign import ccall safe "get_streamBinary_ptr" streamBinary_ptr
  :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4096) HsBindgen.Runtime.Prelude.Word8)

foreign import ccall safe "get_streamBinary_len_ptr" streamBinary_len_ptr
  :: F.Ptr HsBindgen.Runtime.Prelude.Word32

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

foreign import ccall safe "get_some_global_struct_ptr" some_global_struct_ptr
  :: F.Ptr Struct2_t

{-| Constant

  Although this is a constant, we don't expect an initializer (since it's `extern`).

  __from C:__ @globalConstant@
-}
foreign import ccall safe "get_globalConstant_ptr" globalConstant_ptr
  :: F.Ptr FC.CInt

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

foreign import ccall safe "get_anotherGlobalConstant_ptr" anotherGlobalConstant_ptr
  :: F.Ptr ConstInt

{-# NOINLINE anotherGlobalConstant #-}

anotherGlobalConstant :: ConstInt
anotherGlobalConstant =
  GHC.IO.Unsafe.unsafePerformIO (F.peek anotherGlobalConstant_ptr)

{-| Constant, but local to the file

  Unlike with `extern`, in this we _do_ expect an initializer.

  __from C:__ @staticConst@
-}
foreign import ccall safe "get_staticConst_ptr" staticConst_ptr
  :: F.Ptr FC.CInt

{-# NOINLINE staticConst #-}

staticConst :: FC.CInt
staticConst =
  GHC.IO.Unsafe.unsafePerformIO (F.peek staticConst_ptr)

{-| No storage class specified

  __from C:__ @classless@
-}
foreign import ccall safe "get_classless_ptr" classless_ptr
  :: F.Ptr FC.CInt

{-# NOINLINE classless #-}

classless :: FC.CInt
classless =
  GHC.IO.Unsafe.unsafePerformIO (F.peek classless_ptr)

{-| A an array of size 4 containing constant integers

  __from C:__ @constArray1@
-}
foreign import ccall safe "get_constArray1_ptr" constArray1_ptr
  :: F.Ptr ((HsBindgen.Runtime.ConstantArray.ConstantArray 4) FC.CInt)

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

foreign import ccall safe "get_constArray2_ptr" constArray2_ptr
  :: F.Ptr ConstIntArray

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
foreign import ccall safe "get_constTuple_ptr" constTuple_ptr
  :: F.Ptr Tuple

{-# NOINLINE constTuple #-}

constTuple :: Tuple
constTuple =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constTuple_ptr)

{-| A non-constant tuple with a constant member

  __from C:__ @nonConstTuple@
-}
foreign import ccall safe "get_nonConstTuple_ptr" nonConstTuple_ptr
  :: F.Ptr Tuple

{-| A pointer to const int

  __from C:__ @ptrToConstInt@
-}
foreign import ccall safe "get_ptrToConstInt_ptr" ptrToConstInt_ptr
  :: F.Ptr (F.Ptr FC.CInt)

{-| A const pointer to int

  __from C:__ @constPtrToInt@
-}
foreign import ccall safe "get_constPtrToInt_ptr" constPtrToInt_ptr
  :: F.Ptr (F.Ptr FC.CInt)

{-# NOINLINE constPtrToInt #-}

constPtrToInt :: F.Ptr FC.CInt
constPtrToInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToInt_ptr)

{-| A const pointer to const int

  __from C:__ @constPtrToConstInt@
-}
foreign import ccall safe "get_constPtrToConstInt_ptr" constPtrToConstInt_ptr
  :: F.Ptr (F.Ptr FC.CInt)

{-# NOINLINE constPtrToConstInt #-}

constPtrToConstInt :: F.Ptr FC.CInt
constPtrToConstInt =
  GHC.IO.Unsafe.unsafePerformIO (F.peek constPtrToConstInt_ptr)
