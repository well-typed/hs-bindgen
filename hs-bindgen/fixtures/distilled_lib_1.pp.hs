{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.IO.Unsafe
import qualified GHC.Ptr as F
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.IncompleteArray
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)
import qualified Text.Read

$(CAPI.addCSource "#include <distilled_lib_1.h>\nint32_t hs_bindgen_test_distilled_lib_1_a1099223f16f8637 (a_type_t *arg1, uint32_t arg2, uint8_t *arg3) { return some_fun(arg1, arg2, arg3); }\n/* get_some_fun_ptr */ __attribute__ ((const)) int32_t (*hs_bindgen_test_distilled_lib_1_4a8e737205def139 (void)) (a_type_t *arg1, uint32_t arg2, uint8_t arg3[]) { return &some_fun; } \n/* get_v_ptr */ __attribute__ ((const)) var_t *hs_bindgen_test_distilled_lib_1_0f967c83f73d0365 (void) { return &v; } \n")

data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: FC.CInt
  , another_typedef_struct_t_bar :: FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable Another_typedef_struct_t where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Another_typedef_struct_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_struct_t
            another_typedef_struct_t_foo2
            another_typedef_struct_t_bar3 ->
                 F.pokeByteOff ptr0 (0 :: Int) another_typedef_struct_t_foo2
              >> F.pokeByteOff ptr0 (4 :: Int) another_typedef_struct_t_bar3

newtype Another_typedef_enum_e = Another_typedef_enum_e
  { un_Another_typedef_enum_e :: FC.CUInt
  }
  deriving stock (Eq, Ord)

instance F.Storable Another_typedef_enum_e where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Another_typedef_enum_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_enum_e un_Another_typedef_enum_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_Another_typedef_enum_e2

instance HsBindgen.Runtime.CEnum.CEnum Another_typedef_enum_e where

  type CEnumZ Another_typedef_enum_e = FC.CUInt

  toCEnum = Another_typedef_enum_e

  fromCEnum = un_Another_typedef_enum_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "FOO")
                                                     , (1, Data.List.NonEmpty.singleton "BAR")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Another_typedef_enum_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Another_typedef_enum_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Another_typedef_enum_e where

  minDeclaredValue = FOO

  maxDeclaredValue = BAR

instance Show Another_typedef_enum_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Another_typedef_enum_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern FOO :: Another_typedef_enum_e
pattern FOO = Another_typedef_enum_e 0

pattern BAR :: Another_typedef_enum_e
pattern BAR = Another_typedef_enum_e 1

a :: FC.CInt
a = (5 :: FC.CInt)

b :: FC.CInt
b = (3 :: FC.CInt)

sOME_DEFINED_CONSTANT :: FC.CInt
sOME_DEFINED_CONSTANT = (4 :: FC.CInt)

newtype A_type_t = A_type_t
  { un_A_type_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Var_t = Var_t
  { un_Var_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data A_typedef_struct_t = A_typedef_struct_t
  { a_typedef_struct_t_field_0 :: FC.CBool
  , a_typedef_struct_t_field_1 :: HsBindgen.Runtime.Prelude.Word8
  , a_typedef_struct_t_field_2 :: HsBindgen.Runtime.Prelude.Word16
  , a_typedef_struct_t_field_3 :: HsBindgen.Runtime.Prelude.Word32
  , a_typedef_struct_t_field_4 :: Another_typedef_struct_t
  , a_typedef_struct_t_field_5 :: F.Ptr Another_typedef_struct_t
  , a_typedef_struct_t_field_6 :: F.Ptr Void
  , a_typedef_struct_t_field_7 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 7) HsBindgen.Runtime.Prelude.Word32
  , a_typedef_struct_t_field_8 :: Another_typedef_enum_e
  , a_typedef_struct_t_field_9 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) Another_typedef_enum_e
  , a_typedef_struct_t_field_10 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) Another_typedef_enum_e)
  }
  deriving stock (Eq, Show)

instance F.Storable A_typedef_struct_t where

  sizeOf = \_ -> (140 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure A_typedef_struct_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (60 :: Int)
      <*> F.peekByteOff ptr0 (64 :: Int)
      <*> F.peekByteOff ptr0 (80 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_struct_t
            a_typedef_struct_t_field_02
            a_typedef_struct_t_field_13
            a_typedef_struct_t_field_24
            a_typedef_struct_t_field_35
            a_typedef_struct_t_field_46
            a_typedef_struct_t_field_57
            a_typedef_struct_t_field_68
            a_typedef_struct_t_field_79
            a_typedef_struct_t_field_810
            a_typedef_struct_t_field_911
            a_typedef_struct_t_field_1012 ->
                 F.pokeByteOff ptr0 (0 :: Int) a_typedef_struct_t_field_02
              >> F.pokeByteOff ptr0 (1 :: Int) a_typedef_struct_t_field_13
              >> F.pokeByteOff ptr0 (2 :: Int) a_typedef_struct_t_field_24
              >> F.pokeByteOff ptr0 (4 :: Int) a_typedef_struct_t_field_35
              >> F.pokeByteOff ptr0 (8 :: Int) a_typedef_struct_t_field_46
              >> F.pokeByteOff ptr0 (16 :: Int) a_typedef_struct_t_field_57
              >> F.pokeByteOff ptr0 (24 :: Int) a_typedef_struct_t_field_68
              >> F.pokeByteOff ptr0 (32 :: Int) a_typedef_struct_t_field_79
              >> F.pokeByteOff ptr0 (60 :: Int) a_typedef_struct_t_field_810
              >> F.pokeByteOff ptr0 (64 :: Int) a_typedef_struct_t_field_911
              >> F.pokeByteOff ptr0 (80 :: Int) a_typedef_struct_t_field_1012

a_DEFINE_0 :: FC.CInt
a_DEFINE_0 = (0 :: FC.CInt)

a_DEFINE_1 :: FC.CUInt
a_DEFINE_1 = (20560 :: FC.CUInt)

a_DEFINE_2 :: FC.CInt
a_DEFINE_2 = (2 :: FC.CInt)

tWO_ARGS :: ((,) FC.CInt) FC.CInt
tWO_ARGS = (,) (13398 :: FC.CInt) (30874 :: FC.CInt)

newtype A_typedef_enum_e = A_typedef_enum_e
  { un_A_typedef_enum_e :: FC.CUChar
  }
  deriving stock (Eq, Ord)

instance F.Storable A_typedef_enum_e where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure A_typedef_enum_e
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_enum_e un_A_typedef_enum_e2 ->
            F.pokeByteOff ptr0 (0 :: Int) un_A_typedef_enum_e2

instance HsBindgen.Runtime.CEnum.CEnum A_typedef_enum_e where

  type CEnumZ A_typedef_enum_e = FC.CUChar

  toCEnum = A_typedef_enum_e

  fromCEnum = un_A_typedef_enum_e

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "ENUM_CASE_0")
                                                     , (1, Data.List.NonEmpty.singleton "ENUM_CASE_1")
                                                     , (2, Data.List.NonEmpty.singleton "ENUM_CASE_2")
                                                     , (3, Data.List.NonEmpty.singleton "ENUM_CASE_3")
                                                     ]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "A_typedef_enum_e"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "A_typedef_enum_e"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum A_typedef_enum_e where

  minDeclaredValue = ENUM_CASE_0

  maxDeclaredValue = ENUM_CASE_3

instance Show A_typedef_enum_e where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read A_typedef_enum_e where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern ENUM_CASE_0 :: A_typedef_enum_e
pattern ENUM_CASE_0 = A_typedef_enum_e 0

pattern ENUM_CASE_1 :: A_typedef_enum_e
pattern ENUM_CASE_1 = A_typedef_enum_e 1

pattern ENUM_CASE_2 :: A_typedef_enum_e
pattern ENUM_CASE_2 = A_typedef_enum_e 2

pattern ENUM_CASE_3 :: A_typedef_enum_e
pattern ENUM_CASE_3 = A_typedef_enum_e 3

{-| __from C:__ @some_fun@ -}
foreign import ccall safe "hs_bindgen_test_distilled_lib_1_a1099223f16f8637" some_fun_wrapper
  :: F.Ptr A_type_t
     {- ^ __from C:__ @i@ -}
  -> HsBindgen.Runtime.Prelude.Word32
     {- ^ __from C:__ @j@ -}
  -> F.Ptr HsBindgen.Runtime.Prelude.Word8
     {- ^ __from C:__ @k@ -}
  -> IO HsBindgen.Runtime.Prelude.Int32

some_fun :: (F.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32
some_fun =
  \x0 ->
    \x1 ->
      \x2 ->
        HsBindgen.Runtime.IncompleteArray.withPtr x2 (\ptr3 ->
                                                        some_fun_wrapper x0 x1 ptr3)

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_4a8e737205def139" hs_bindgen_test_distilled_lib_1_4a8e737205def139
  :: IO (F.FunPtr ((F.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32))

{-# NOINLINE some_fun_ptr #-}

some_fun_ptr :: F.FunPtr ((F.Ptr A_type_t) -> HsBindgen.Runtime.Prelude.Word32 -> (HsBindgen.Runtime.IncompleteArray.IncompleteArray HsBindgen.Runtime.Prelude.Word8) -> IO HsBindgen.Runtime.Prelude.Int32)
some_fun_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_4a8e737205def139

newtype Callback_t = Callback_t
  { un_Callback_t :: F.FunPtr ((F.Ptr Void) -> HsBindgen.Runtime.Prelude.Word32 -> IO HsBindgen.Runtime.Prelude.Word32)
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

foreign import ccall unsafe "hs_bindgen_test_distilled_lib_1_0f967c83f73d0365" hs_bindgen_test_distilled_lib_1_0f967c83f73d0365
  :: IO (F.Ptr Var_t)

{-# NOINLINE v_ptr #-}

v_ptr :: F.Ptr Var_t
v_ptr =
  GHC.IO.Unsafe.unsafePerformIO hs_bindgen_test_distilled_lib_1_0f967c83f73d0365
