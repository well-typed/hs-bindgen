{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Data.Map.Strict
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, show)

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

deriving stock instance Eq First

deriving stock instance Ord First

deriving stock instance Read First

instance HsBindgen.Runtime.CEnum.CEnum First where

  type CEnumZ First = FC.CUInt

  fromCEnumZ = First

  toCEnumZ = un_First

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "FIRST1"), (1, pure "FIRST2")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum First where

  minValue = First 0

  maxValue = First 1

instance Show First where

  show = HsBindgen.Runtime.CEnum.showCEnum "First"

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

deriving stock instance Eq Second

deriving stock instance Ord Second

deriving stock instance Read Second

instance HsBindgen.Runtime.CEnum.CEnum Second where

  type CEnumZ Second = FC.CInt

  fromCEnumZ = Second

  toCEnumZ = un_Second

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(-1, pure "SECOND_A"), (0, pure "SECOND_B"), (1, pure "SECOND_C")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum Second where

  minValue = Second (-1)

  maxValue = Second 1

instance Show Second where

  show = HsBindgen.Runtime.CEnum.showCEnum "Second"

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

deriving stock instance Eq Same

deriving stock instance Ord Same

deriving stock instance Read Same

instance HsBindgen.Runtime.CEnum.CEnum Same where

  type CEnumZ Same = FC.CUInt

  fromCEnumZ = Same

  toCEnumZ = un_Same

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(1, ("SAME_B" Data.List.NonEmpty.:| ["SAME_A"]))]

instance HsBindgen.Runtime.CEnum.SequentialCEnum Same where

  minValue = Same 1

  maxValue = Same 1

instance Show Same where

  show = HsBindgen.Runtime.CEnum.showCEnum "Same"

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

deriving stock instance Eq Nonseq

deriving stock instance Ord Nonseq

deriving stock instance Read Nonseq

instance HsBindgen.Runtime.CEnum.CEnum Nonseq where

  type CEnumZ Nonseq = FC.CUInt

  fromCEnumZ = Nonseq

  toCEnumZ = un_Nonseq

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(200, pure "NONSEQ_A"), (301, pure "NONSEQ_B"), (404, pure "NONSEQ_C")]

instance Show Nonseq where

  show = HsBindgen.Runtime.CEnum.showCEnum "Nonseq"

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

deriving stock instance Eq Packad

deriving stock instance Ord Packad

deriving stock instance Read Packad

instance HsBindgen.Runtime.CEnum.CEnum Packad where

  type CEnumZ Packad = FC.CSChar

  fromCEnumZ = Packad

  toCEnumZ = un_Packad

  declaredValues =
    \_ ->
      Data.Map.Strict.fromList [(0, pure "PACKED_A"), (1, pure "PACKED_B"), (2, pure "PACKED_C")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum Packad where

  minValue = Packad 0

  maxValue = Packad 2

instance Show Packad where

  show = HsBindgen.Runtime.CEnum.showCEnum "Packad"

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

deriving stock instance Eq EnumA

deriving stock instance Ord EnumA

deriving stock instance Read EnumA

instance HsBindgen.Runtime.CEnum.CEnum EnumA where

  type CEnumZ EnumA = FC.CUInt

  fromCEnumZ = EnumA

  toCEnumZ = un_EnumA

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "A_FOO"), (1, pure "A_BAR")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minValue = EnumA 0

  maxValue = EnumA 1

instance Show EnumA where

  show = HsBindgen.Runtime.CEnum.showCEnum "EnumA"

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

deriving stock instance Eq EnumB

deriving stock instance Ord EnumB

deriving stock instance Read EnumB

instance HsBindgen.Runtime.CEnum.CEnum EnumB where

  type CEnumZ EnumB = FC.CUInt

  fromCEnumZ = EnumB

  toCEnumZ = un_EnumB

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "B_FOO"), (1, pure "B_BAR")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumB where

  minValue = EnumB 0

  maxValue = EnumB 1

instance Show EnumB where

  show = HsBindgen.Runtime.CEnum.showCEnum "EnumB"

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

deriving stock instance Eq EnumC

deriving stock instance Ord EnumC

deriving stock instance Read EnumC

instance HsBindgen.Runtime.CEnum.CEnum EnumC where

  type CEnumZ EnumC = FC.CUInt

  fromCEnumZ = EnumC

  toCEnumZ = un_EnumC

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "C_FOO"), (1, pure "C_BAR")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumC where

  minValue = EnumC 0

  maxValue = EnumC 1

instance Show EnumC where

  show = HsBindgen.Runtime.CEnum.showCEnum "EnumC"

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

deriving stock instance Eq EnumD

deriving stock instance Ord EnumD

deriving stock instance Read EnumD

instance HsBindgen.Runtime.CEnum.CEnum EnumD where

  type CEnumZ EnumD = FC.CUInt

  fromCEnumZ = EnumD

  toCEnumZ = un_EnumD

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "D_FOO"), (1, pure "D_BAR")]

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumD where

  minValue = EnumD 0

  maxValue = EnumD 1

instance Show EnumD where

  show = HsBindgen.Runtime.CEnum.showCEnum "EnumD"

pattern D_FOO :: EnumD
pattern D_FOO = EnumD 0

pattern D_BAR :: EnumD
pattern D_BAR = EnumD 1

newtype EnumD_t = EnumD_t
  { un_EnumD_t :: EnumD
  }

deriving newtype instance F.Storable EnumD_t
