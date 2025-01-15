{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.ConstantArray
import Prelude ((<*>), (>>), IO, pure)

a :: FC.CInt
a = 5

b :: FC.CInt
b = 3

sOME_DEFINED_CONSTANT :: FC.CInt
sOME_DEFINED_CONSTANT = 4

a_DEFINE_0 :: FC.CInt
a_DEFINE_0 = 0

a_DEFINE_1 :: FC.CUInt
a_DEFINE_1 = 20560

a_DEFINE_2 :: FC.CInt
a_DEFINE_2 = 2

tWO_ARGS :: FC.CInt
tWO_ARGS = 13398

foreign import capi safe "distilled_lib_1.h some_fun" some_fun :: (F.Ptr A_type_t) -> Uint32_t -> Void -> IO Int32_t

data Another_typedef_struct_t = Another_typedef_struct_t
  { another_typedef_struct_t_foo :: FC.CInt
  , another_typedef_struct_t_bar :: FC.CChar
  }

instance F.Storable Another_typedef_struct_t where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Another_typedef_struct_t
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_struct_t another_typedef_struct_t_foo2 another_typedef_struct_t_bar3 ->
               F.pokeByteOff ptr0 0 another_typedef_struct_t_foo2
            >> F.pokeByteOff ptr0 4 another_typedef_struct_t_bar3

newtype Another_typedef_enum_e = Another_typedef_enum_e
  { unAnother_typedef_enum_e :: FC.CUInt
  }

instance F.Storable Another_typedef_enum_e where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Another_typedef_enum_e
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Another_typedef_enum_e unAnother_typedef_enum_e2 ->
            F.pokeByteOff ptr0 0 unAnother_typedef_enum_e2

pattern FOO :: Another_typedef_enum_e
pattern FOO = Another_typedef_enum_e 0

pattern BAR :: Another_typedef_enum_e
pattern BAR = Another_typedef_enum_e 1

newtype A_type_t = A_type_t
  { unA_type_t :: FC.CInt
  }

deriving newtype instance F.Storable A_type_t

newtype Var_t = Var_t
  { unVar_t :: FC.CInt
  }

deriving newtype instance F.Storable Var_t

newtype Uint8_t = Uint8_t
  { unUint8_t :: FC.CSChar
  }

deriving newtype instance F.Storable Uint8_t

newtype Uint16_t = Uint16_t
  { unUint16_t :: FC.CUShort
  }

deriving newtype instance F.Storable Uint16_t

newtype Uint32_t = Uint32_t
  { unUint32_t :: FC.CUInt
  }

deriving newtype instance F.Storable Uint32_t

data A_typedef_struct = A_typedef_struct
  { a_typedef_struct_field_0 :: FC.CBool
  , a_typedef_struct_field_1 :: Uint8_t
  , a_typedef_struct_field_2 :: Uint16_t
  , a_typedef_struct_field_3 :: Uint32_t
  , a_typedef_struct_field_4 :: Another_typedef_struct_t
  , a_typedef_struct_field_5 :: F.Ptr Another_typedef_struct_t
  , a_typedef_struct_field_6 :: F.Ptr Void
  , a_typedef_struct_field_7 :: (HsBindgen.ConstantArray.ConstantArray 7) Uint32_t
  , a_typedef_struct_field_8 :: Another_typedef_enum_e
  , a_typedef_struct_field_9 :: Another_typedef_enum_e
  , a_typedef_struct_field_10 :: Another_typedef_enum_e
  }

instance F.Storable A_typedef_struct where

  sizeOf = \_ -> 140

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure A_typedef_struct
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1
      <*> F.peekByteOff ptr0 2
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8
      <*> F.peekByteOff ptr0 16
      <*> F.peekByteOff ptr0 24
      <*> F.peekByteOff ptr0 32
      <*> F.peekByteOff ptr0 60
      <*> F.peekByteOff ptr0 64
      <*> F.peekByteOff ptr0 80

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
                 F.pokeByteOff ptr0 0 a_typedef_struct_field_02
              >> F.pokeByteOff ptr0 1 a_typedef_struct_field_13
              >> F.pokeByteOff ptr0 2 a_typedef_struct_field_24
              >> F.pokeByteOff ptr0 4 a_typedef_struct_field_35
              >> F.pokeByteOff ptr0 8 a_typedef_struct_field_46
              >> F.pokeByteOff ptr0 16 a_typedef_struct_field_57
              >> F.pokeByteOff ptr0 24 a_typedef_struct_field_68
              >> F.pokeByteOff ptr0 32 a_typedef_struct_field_79
              >> F.pokeByteOff ptr0 60 a_typedef_struct_field_810
              >> F.pokeByteOff ptr0 64 a_typedef_struct_field_911
              >> F.pokeByteOff ptr0 80 a_typedef_struct_field_1012

newtype A_typedef_struct_t = A_typedef_struct_t
  { unA_typedef_struct_t :: A_typedef_struct
  }

deriving newtype instance F.Storable A_typedef_struct_t

newtype A_typedef_enum_e = A_typedef_enum_e
  { unA_typedef_enum_e :: FC.CSChar
  }

instance F.Storable A_typedef_enum_e where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure A_typedef_enum_e
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          A_typedef_enum_e unA_typedef_enum_e2 -> F.pokeByteOff ptr0 0 unA_typedef_enum_e2

pattern ENUM_CASE_0 :: A_typedef_enum_e
pattern ENUM_CASE_0 = A_typedef_enum_e 0

pattern ENUM_CASE_1 :: A_typedef_enum_e
pattern ENUM_CASE_1 = A_typedef_enum_e 1

pattern ENUM_CASE_2 :: A_typedef_enum_e
pattern ENUM_CASE_2 = A_typedef_enum_e 2

pattern ENUM_CASE_3 :: A_typedef_enum_e
pattern ENUM_CASE_3 = A_typedef_enum_e 3

newtype Int32_t = Int32_t
  { unInt32_t :: FC.CInt
  }

deriving newtype instance F.Storable Int32_t

newtype Callback_t = Callback_t
  { unCallback_t :: F.FunPtr ((F.Ptr Void) -> Uint32_t -> IO Uint32_t)
  }

deriving newtype instance F.Storable Callback_t
