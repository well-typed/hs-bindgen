{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum.General
import qualified HsBindgen.Runtime.CEnum.Sequential
import Prelude ((<*>), Bounded, Enum, Eq, Int, Ord, Read, Show, pure)

newtype First = First
  { un_First :: FC.CUInt
  }

instance F.Storable First where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure First
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          First un_First2 -> F.pokeByteOff ptr0 (0 :: Int) un_First2

deriving stock instance Show First

deriving stock instance Read First

deriving stock instance Eq First

deriving stock instance Ord First

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum First where

  type SequentialCEnumZ First = FC.CUInt

  toSequentialCEnum = First

  fromSequentialCEnum = un_First

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum First instance Bounded First

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum First instance Enum First

pattern FIRST1 :: First
pattern FIRST1 = First 0

pattern FIRST2 :: First
pattern FIRST2 = First 1

newtype Second = Second
  { un_Second :: FC.CInt
  }

instance F.Storable Second where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Second
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Second un_Second2 -> F.pokeByteOff ptr0 (0 :: Int) un_Second2

deriving stock instance Show Second

deriving stock instance Read Second

deriving stock instance Eq Second

deriving stock instance Ord Second

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum Second where

  type SequentialCEnumZ Second = FC.CInt

  toSequentialCEnum = Second

  fromSequentialCEnum = un_Second

  sequentialCEnumMin = \_ -> -1

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Second instance Bounded Second

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Second instance Enum Second

pattern SECOND_A :: Second
pattern SECOND_A = Second (-1)

pattern SECOND_B :: Second
pattern SECOND_B = Second 0

pattern SECOND_C :: Second
pattern SECOND_C = Second 1

newtype Same = Same
  { un_Same :: FC.CUInt
  }

instance F.Storable Same where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Same
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same un_Same2 -> F.pokeByteOff ptr0 (0 :: Int) un_Same2

deriving stock instance Show Same

deriving stock instance Read Same

deriving stock instance Eq Same

deriving stock instance Ord Same

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum Same where

  type SequentialCEnumZ Same = FC.CUInt

  toSequentialCEnum = Same

  fromSequentialCEnum = un_Same

  sequentialCEnumMin = \_ -> 1

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Same instance Bounded Same

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Same instance Enum Same

pattern SAME_A :: Same
pattern SAME_A = Same 1

pattern SAME_B :: Same
pattern SAME_B = Same 1

newtype Nonseq = Nonseq
  { un_Nonseq :: FC.CUInt
  }

instance F.Storable Nonseq where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Nonseq
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Nonseq un_Nonseq2 -> F.pokeByteOff ptr0 (0 :: Int) un_Nonseq2

deriving stock instance Show Nonseq

deriving stock instance Read Nonseq

deriving stock instance Eq Nonseq

deriving stock instance Ord Nonseq

instance HsBindgen.Runtime.CEnum.General.GeneralCEnum Nonseq where

  type GeneralCEnumZ Nonseq = FC.CUInt

  toGeneralCEnum = Nonseq

  fromGeneralCEnum = un_Nonseq

  generalCEnumValues =
    \_ ->
      [ 200
      , 301
      , 404
      ]

deriving via HsBindgen.Runtime.CEnum.General.GenCEnum Nonseq instance Bounded Nonseq

deriving via HsBindgen.Runtime.CEnum.General.GenCEnum Nonseq instance Enum Nonseq

pattern NONSEQ_A :: Nonseq
pattern NONSEQ_A = Nonseq 200

pattern NONSEQ_B :: Nonseq
pattern NONSEQ_B = Nonseq 301

pattern NONSEQ_C :: Nonseq
pattern NONSEQ_C = Nonseq 404

newtype Packad = Packad
  { un_Packad :: FC.CSChar
  }

instance F.Storable Packad where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Packad
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Packad un_Packad2 -> F.pokeByteOff ptr0 (0 :: Int) un_Packad2

deriving stock instance Show Packad

deriving stock instance Read Packad

deriving stock instance Eq Packad

deriving stock instance Ord Packad

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum Packad where

  type SequentialCEnumZ Packad = FC.CSChar

  toSequentialCEnum = Packad

  fromSequentialCEnum = un_Packad

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 2

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Packad instance Bounded Packad

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Packad instance Enum Packad

pattern PACKED_A :: Packad
pattern PACKED_A = Packad 0

pattern PACKED_B :: Packad
pattern PACKED_B = Packad 1

pattern PACKED_C :: Packad
pattern PACKED_C = Packad 2

newtype EnumA = EnumA
  { un_EnumA :: FC.CUInt
  }

instance F.Storable EnumA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure EnumA
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA un_EnumA2 -> F.pokeByteOff ptr0 (0 :: Int) un_EnumA2

deriving stock instance Show EnumA

deriving stock instance Read EnumA

deriving stock instance Eq EnumA

deriving stock instance Ord EnumA

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum EnumA where

  type SequentialCEnumZ EnumA = FC.CUInt

  toSequentialCEnum = EnumA

  fromSequentialCEnum = un_EnumA

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumA instance Bounded EnumA

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumA instance Enum EnumA

pattern A_FOO :: EnumA
pattern A_FOO = EnumA 0

pattern A_BAR :: EnumA
pattern A_BAR = EnumA 1

newtype EnumB = EnumB
  { un_EnumB :: FC.CUInt
  }

instance F.Storable EnumB where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure EnumB
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumB un_EnumB2 -> F.pokeByteOff ptr0 (0 :: Int) un_EnumB2

deriving stock instance Show EnumB

deriving stock instance Read EnumB

deriving stock instance Eq EnumB

deriving stock instance Ord EnumB

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum EnumB where

  type SequentialCEnumZ EnumB = FC.CUInt

  toSequentialCEnum = EnumB

  fromSequentialCEnum = un_EnumB

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumB instance Bounded EnumB

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumB instance Enum EnumB

pattern B_FOO :: EnumB
pattern B_FOO = EnumB 0

pattern B_BAR :: EnumB
pattern B_BAR = EnumB 1

newtype EnumC = EnumC
  { un_EnumC :: FC.CUInt
  }

instance F.Storable EnumC where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure EnumC
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumC un_EnumC2 -> F.pokeByteOff ptr0 (0 :: Int) un_EnumC2

deriving stock instance Show EnumC

deriving stock instance Read EnumC

deriving stock instance Eq EnumC

deriving stock instance Ord EnumC

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum EnumC where

  type SequentialCEnumZ EnumC = FC.CUInt

  toSequentialCEnum = EnumC

  fromSequentialCEnum = un_EnumC

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumC instance Bounded EnumC

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumC instance Enum EnumC

pattern C_FOO :: EnumC
pattern C_FOO = EnumC 0

pattern C_BAR :: EnumC
pattern C_BAR = EnumC 1

newtype EnumD = EnumD
  { un_EnumD :: FC.CUInt
  }

instance F.Storable EnumD where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure EnumD
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumD un_EnumD2 -> F.pokeByteOff ptr0 (0 :: Int) un_EnumD2

deriving stock instance Show EnumD

deriving stock instance Read EnumD

deriving stock instance Eq EnumD

deriving stock instance Ord EnumD

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum EnumD where

  type SequentialCEnumZ EnumD = FC.CUInt

  toSequentialCEnum = EnumD

  fromSequentialCEnum = un_EnumD

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumD instance Bounded EnumD

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum EnumD instance Enum EnumD

pattern D_FOO :: EnumD
pattern D_FOO = EnumD 0

pattern D_BAR :: EnumD
pattern D_BAR = EnumD 1

newtype EnumD_t = EnumD_t
  { un_EnumD_t :: EnumD
  }

deriving newtype instance F.Storable EnumD_t
