{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Array.Byte
import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

data S = S
  { s_f :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort
  }
  deriving stock (Eq, Show)

instance F.Storable S where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S s_f2 -> F.pokeByteOff ptr0 (0 :: Int) s_f2

newtype More_aligned_int = More_aligned_int
  { un_More_aligned_int :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data S2 = S2
  { s2_f :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CShort
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (16 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_f2 -> F.pokeByteOff ptr0 (0 :: Int) s2_f2

data My_unpacked_struct = My_unpacked_struct
  { my_unpacked_struct_c :: FC.CChar
  , my_unpacked_struct_i :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable My_unpacked_struct where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure My_unpacked_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_unpacked_struct my_unpacked_struct_c2 my_unpacked_struct_i3 ->
               F.pokeByteOff ptr0 (0 :: Int) my_unpacked_struct_c2
            >> F.pokeByteOff ptr0 (4 :: Int) my_unpacked_struct_i3

data My_packed_struct = My_packed_struct
  { my_packed_struct_c :: FC.CChar
  , my_packed_struct_i :: FC.CInt
  , my_packed_struct_s :: My_unpacked_struct
  }
  deriving stock (Eq, Show)

instance F.Storable My_packed_struct where

  sizeOf = \_ -> (13 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure My_packed_struct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)
      <*> F.peekByteOff ptr0 (5 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          My_packed_struct my_packed_struct_c2 my_packed_struct_i3 my_packed_struct_s4 ->
               F.pokeByteOff ptr0 (0 :: Int) my_packed_struct_c2
            >> F.pokeByteOff ptr0 (1 :: Int) my_packed_struct_i3
            >> F.pokeByteOff ptr0 (5 :: Int) my_packed_struct_s4

data Wait

newtype Wait_status_ptr_t = Wait_status_ptr_t
  { un_Wait_status_ptr_t :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 8 instance F.Storable Wait_status_ptr_t

get_wait_status_ptr_t___ip :: Wait_status_ptr_t -> F.Ptr FC.CInt
get_wait_status_ptr_t___ip =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_wait_status_ptr_t___ip :: (F.Ptr FC.CInt) -> Wait_status_ptr_t
set_wait_status_ptr_t___ip =
  HsBindgen.Runtime.ByteArray.setUnionPayload

get_wait_status_ptr_t___up :: Wait_status_ptr_t -> F.Ptr Wait
get_wait_status_ptr_t___up =
  HsBindgen.Runtime.ByteArray.getUnionPayload

set_wait_status_ptr_t___up :: (F.Ptr Wait) -> Wait_status_ptr_t
set_wait_status_ptr_t___up =
  HsBindgen.Runtime.ByteArray.setUnionPayload

newtype T1 = T1
  { un_T1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype Short_a = Short_a
  { un_Short_a :: FC.CShort
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)
