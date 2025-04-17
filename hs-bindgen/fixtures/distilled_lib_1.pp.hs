{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Map.Strict
import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, show)

a :: FC.CInt
a = (5 :: FC.CInt)

b :: FC.CInt
b = (3 :: FC.CInt)

sOME_DEFINED_CONSTANT :: FC.CInt
sOME_DEFINED_CONSTANT = (4 :: FC.CInt)

a_DEFINE_0 :: FC.CInt
a_DEFINE_0 = (0 :: FC.CInt)

a_DEFINE_1 :: FC.CUInt
a_DEFINE_1 = (20560 :: FC.CUInt)

a_DEFINE_2 :: FC.CInt
a_DEFINE_2 = (2 :: FC.CInt)

tWO_ARGS :: ((,) FC.CInt) FC.CInt
tWO_ARGS = (,) (13398 :: FC.CInt) (30874 :: FC.CInt)

foreign import capi safe "distilled_lib_1.h some_fun" some_fun :: (F.Ptr A_type_t) -> Uint32_t -> (F.Ptr Uint8_t) -> IO Int32_t

data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: FC.CInt
  , another_typedef_struct_t_bar :: FC.CChar
  }

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
          Another_typedef_struct_t another_typedef_struct_t_foo2 another_typedef_struct_t_bar3 ->
               F.pokeByteOff ptr0 (0 :: Int) another_typedef_struct_t_foo2
            >> F.pokeByteOff ptr0 (4 :: Int) another_typedef_struct_t_bar3

deriving stock instance Show Another_typedef_struct_t

deriving stock instance Eq Another_typedef_struct_t

newtype Another_typedef_enum_e = Another_typedef_enum_e
  { un_Another_typedef_enum_e :: FC.CUInt
  }

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

deriving stock instance Eq Another_typedef_enum_e

deriving stock instance Ord Another_typedef_enum_e

deriving stock instance Read Another_typedef_enum_e

instance HsBindgen.Runtime.CEnum.CEnum Another_typedef_enum_e where

  type CEnumZ Another_typedef_enum_e = FC.CUInt

  fromCEnumZ = Another_typedef_enum_e

  toCEnumZ = un_Another_typedef_enum_e

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(0, Data.List.NonEmpty.singleton "FOO"), (1, Data.List.NonEmpty.singleton "BAR")]

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Another_typedef_enum_e where

  minDeclaredValue = FOO

  maxDeclaredValue = BAR

instance Show Another_typedef_enum_e where

  show = HsBindgen.Runtime.CEnum.showCEnum "Another_typedef_enum_e"

pattern FOO :: Another_typedef_enum_e
pattern FOO = Another_typedef_enum_e 0

pattern BAR :: Another_typedef_enum_e
pattern BAR = Another_typedef_enum_e 1

newtype A_type_t = A_type_t
  { un_A_type_t :: FC.CInt
  }

deriving newtype instance F.Storable A_type_t

deriving stock instance Eq A_type_t

deriving stock instance Ord A_type_t

deriving stock instance Read A_type_t

deriving stock instance Show A_type_t

deriving newtype instance Enum A_type_t

deriving newtype instance Ix.Ix A_type_t

deriving newtype instance Bounded A_type_t

deriving newtype instance Bits.Bits A_type_t

deriving newtype instance FiniteBits A_type_t

deriving newtype instance Integral A_type_t

deriving newtype instance Num A_type_t

deriving newtype instance Real A_type_t

newtype Var_t = Var_t
  { un_Var_t :: FC.CInt
  }

deriving newtype instance F.Storable Var_t

deriving stock instance Eq Var_t

deriving stock instance Ord Var_t

deriving stock instance Read Var_t

deriving stock instance Show Var_t

deriving newtype instance Enum Var_t

deriving newtype instance Ix.Ix Var_t

deriving newtype instance Bounded Var_t

deriving newtype instance Bits.Bits Var_t

deriving newtype instance FiniteBits Var_t

deriving newtype instance Integral Var_t

deriving newtype instance Num Var_t

deriving newtype instance Real Var_t

newtype Uint8_t = Uint8_t
  { un_Uint8_t :: FC.CSChar
  }

deriving newtype instance F.Storable Uint8_t

deriving stock instance Eq Uint8_t

deriving stock instance Ord Uint8_t

deriving stock instance Read Uint8_t

deriving stock instance Show Uint8_t

deriving newtype instance Enum Uint8_t

deriving newtype instance Ix.Ix Uint8_t

deriving newtype instance Bounded Uint8_t

deriving newtype instance Bits.Bits Uint8_t

deriving newtype instance FiniteBits Uint8_t

deriving newtype instance Integral Uint8_t

deriving newtype instance Num Uint8_t

deriving newtype instance Real Uint8_t

newtype Uint16_t = Uint16_t
  { un_Uint16_t :: FC.CUShort
  }

deriving newtype instance F.Storable Uint16_t

deriving stock instance Eq Uint16_t

deriving stock instance Ord Uint16_t

deriving stock instance Read Uint16_t

deriving stock instance Show Uint16_t

deriving newtype instance Enum Uint16_t

deriving newtype instance Ix.Ix Uint16_t

deriving newtype instance Bounded Uint16_t

deriving newtype instance Bits.Bits Uint16_t

deriving newtype instance FiniteBits Uint16_t

deriving newtype instance Integral Uint16_t

deriving newtype instance Num Uint16_t

deriving newtype instance Real Uint16_t

newtype Uint32_t = Uint32_t
  { un_Uint32_t :: FC.CUInt
  }

deriving newtype instance F.Storable Uint32_t

deriving stock instance Eq Uint32_t

deriving stock instance Ord Uint32_t

deriving stock instance Read Uint32_t

deriving stock instance Show Uint32_t

deriving newtype instance Enum Uint32_t

deriving newtype instance Ix.Ix Uint32_t

deriving newtype instance Bounded Uint32_t

deriving newtype instance Bits.Bits Uint32_t

deriving newtype instance FiniteBits Uint32_t

deriving newtype instance Integral Uint32_t

deriving newtype instance Num Uint32_t

deriving newtype instance Real Uint32_t

data A_typedef_struct = A_typedef_struct
  { a_typedef_struct_field_0 :: FC.CBool
  , a_typedef_struct_field_1 :: Uint8_t
  , a_typedef_struct_field_2 :: Uint16_t
  , a_typedef_struct_field_3 :: Uint32_t
  , a_typedef_struct_field_4 :: Another_typedef_struct_t
  , a_typedef_struct_field_5 :: F.Ptr Another_typedef_struct_t
  , a_typedef_struct_field_6 :: F.Ptr Void
  , a_typedef_struct_field_7 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 7) Uint32_t
  , a_typedef_struct_field_8 :: Another_typedef_enum_e
  , a_typedef_struct_field_9 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 4) Another_typedef_enum_e
  , a_typedef_struct_field_10 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 5) ((HsBindgen.Runtime.ConstantArray.ConstantArray 3) Another_typedef_enum_e)
  }

instance F.Storable A_typedef_struct where

  sizeOf = \_ -> (140 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure A_typedef_struct
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
          A_typedef_struct
            a_typedef_struct_field_02
            a_typedef_struct_field_13
            a_typedef_struct_field_24
            a_typedef_struct_field_35
            a_typedef_struct_field_46
            a_typedef_struct_field_57
            a_typedef_struct_field_68
            a_typedef_struct_field_79
            a_typedef_struct_field_810
            a_typedef_struct_field_911
            a_typedef_struct_field_1012 ->
                 F.pokeByteOff ptr0 (0 :: Int) a_typedef_struct_field_02
              >> F.pokeByteOff ptr0 (1 :: Int) a_typedef_struct_field_13
              >> F.pokeByteOff ptr0 (2 :: Int) a_typedef_struct_field_24
              >> F.pokeByteOff ptr0 (4 :: Int) a_typedef_struct_field_35
              >> F.pokeByteOff ptr0 (8 :: Int) a_typedef_struct_field_46
              >> F.pokeByteOff ptr0 (16 :: Int) a_typedef_struct_field_57
              >> F.pokeByteOff ptr0 (24 :: Int) a_typedef_struct_field_68
              >> F.pokeByteOff ptr0 (32 :: Int) a_typedef_struct_field_79
              >> F.pokeByteOff ptr0 (60 :: Int) a_typedef_struct_field_810
              >> F.pokeByteOff ptr0 (64 :: Int) a_typedef_struct_field_911
              >> F.pokeByteOff ptr0 (80 :: Int) a_typedef_struct_field_1012

deriving stock instance Show A_typedef_struct

deriving stock instance Eq A_typedef_struct

newtype A_typedef_struct_t = A_typedef_struct_t
  { un_A_typedef_struct_t :: A_typedef_struct
  }

deriving newtype instance F.Storable A_typedef_struct_t

newtype A_typedef_enum_e = A_typedef_enum_e
  { un_A_typedef_enum_e :: FC.CSChar
  }

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

deriving stock instance Eq A_typedef_enum_e

deriving stock instance Ord A_typedef_enum_e

deriving stock instance Read A_typedef_enum_e

instance HsBindgen.Runtime.CEnum.CEnum A_typedef_enum_e where

  type CEnumZ A_typedef_enum_e = FC.CSChar

  fromCEnumZ = A_typedef_enum_e

  toCEnumZ = un_A_typedef_enum_e

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [ (0, Data.List.NonEmpty.singleton "ENUM_CASE_0")
                               , (1, Data.List.NonEmpty.singleton "ENUM_CASE_1")
                               , (2, Data.List.NonEmpty.singleton "ENUM_CASE_2")
                               , (3, Data.List.NonEmpty.singleton "ENUM_CASE_3")
                               ]

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum A_typedef_enum_e where

  minDeclaredValue = ENUM_CASE_0

  maxDeclaredValue = ENUM_CASE_3

instance Show A_typedef_enum_e where

  show = HsBindgen.Runtime.CEnum.showCEnum "A_typedef_enum_e"

pattern ENUM_CASE_0 :: A_typedef_enum_e
pattern ENUM_CASE_0 = A_typedef_enum_e 0

pattern ENUM_CASE_1 :: A_typedef_enum_e
pattern ENUM_CASE_1 = A_typedef_enum_e 1

pattern ENUM_CASE_2 :: A_typedef_enum_e
pattern ENUM_CASE_2 = A_typedef_enum_e 2

pattern ENUM_CASE_3 :: A_typedef_enum_e
pattern ENUM_CASE_3 = A_typedef_enum_e 3

newtype Int32_t = Int32_t
  { un_Int32_t :: FC.CInt
  }

deriving newtype instance F.Storable Int32_t

deriving stock instance Eq Int32_t

deriving stock instance Ord Int32_t

deriving stock instance Read Int32_t

deriving stock instance Show Int32_t

deriving newtype instance Enum Int32_t

deriving newtype instance Ix.Ix Int32_t

deriving newtype instance Bounded Int32_t

deriving newtype instance Bits.Bits Int32_t

deriving newtype instance FiniteBits Int32_t

deriving newtype instance Integral Int32_t

deriving newtype instance Num Int32_t

deriving newtype instance Real Int32_t

newtype Callback_t = Callback_t
  { un_Callback_t :: F.FunPtr ((F.Ptr Void) -> Uint32_t -> IO Uint32_t)
  }

deriving newtype instance F.Storable Callback_t
