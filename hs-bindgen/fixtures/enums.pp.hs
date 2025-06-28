{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)
import qualified Text.Read

newtype First = First
  { un_First :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum First where

  type CEnumZ First = FC.CUInt

  toCEnum = First

  fromCEnum = un_First

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "FIRST1"), (1, Data.List.NonEmpty.singleton "FIRST2")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "First"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "First"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum First where

  minDeclaredValue = FIRST1

  maxDeclaredValue = FIRST2

instance Show First where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read First where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern FIRST1 :: First
pattern FIRST1 = First 0

pattern FIRST2 :: First
pattern FIRST2 = First 1

newtype Second = Second
  { un_Second :: FC.CInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum Second where

  type CEnumZ Second = FC.CInt

  toCEnum = Second

  fromCEnum = un_Second

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (-1, Data.List.NonEmpty.singleton "SECOND_A")
                                                     , (0, Data.List.NonEmpty.singleton "SECOND_B")
                                                     , (1, Data.List.NonEmpty.singleton "SECOND_C")
                                                     ]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Second"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Second"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Second where

  minDeclaredValue = SECOND_A

  maxDeclaredValue = SECOND_C

instance Show Second where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Second where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern SECOND_A :: Second
pattern SECOND_A = Second (-1)

pattern SECOND_B :: Second
pattern SECOND_B = Second 0

pattern SECOND_C :: Second
pattern SECOND_C = Second 1

newtype Same = Same
  { un_Same :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum Same where

  type CEnumZ Same = FC.CUInt

  toCEnum = Same

  fromCEnum = un_Same

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(1, ("SAME_A" Data.List.NonEmpty.:| ["SAME_B"]))]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Same"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Same"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Same where

  minDeclaredValue = SAME_A

  maxDeclaredValue = SAME_A

instance Show Same where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Same where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern SAME_A :: Same
pattern SAME_A = Same 1

pattern SAME_B :: Same
pattern SAME_B = Same 1

newtype Nonseq = Nonseq
  { un_Nonseq :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum Nonseq where

  type CEnumZ Nonseq = FC.CUInt

  toCEnum = Nonseq

  fromCEnum = un_Nonseq

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (200, Data.List.NonEmpty.singleton "NONSEQ_A")
                                                     , (301, Data.List.NonEmpty.singleton "NONSEQ_B")
                                                     , (404, Data.List.NonEmpty.singleton "NONSEQ_C")
                                                     ]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Nonseq"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Nonseq"

instance Show Nonseq where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Nonseq where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern NONSEQ_A :: Nonseq
pattern NONSEQ_A = Nonseq 200

pattern NONSEQ_B :: Nonseq
pattern NONSEQ_B = Nonseq 301

pattern NONSEQ_C :: Nonseq
pattern NONSEQ_C = Nonseq 404

newtype Packed = Packed
  { un_Packed :: FC.CUChar
  }
  deriving stock (Eq, Ord)

instance F.Storable Packed where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure Packed
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Packed un_Packed2 -> F.pokeByteOff ptr0 (0 :: Int) un_Packed2

instance HsBindgen.Runtime.CEnum.CEnum Packed where

  type CEnumZ Packed = FC.CUChar

  toCEnum = Packed

  fromCEnum = un_Packed

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [ (0, Data.List.NonEmpty.singleton "PACKED_A")
                                                     , (1, Data.List.NonEmpty.singleton "PACKED_B")
                                                     , (2, Data.List.NonEmpty.singleton "PACKED_C")
                                                     ]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "Packed"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "Packed"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum Packed where

  minDeclaredValue = PACKED_A

  maxDeclaredValue = PACKED_C

instance Show Packed where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read Packed where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern PACKED_A :: Packed
pattern PACKED_A = Packed 0

pattern PACKED_B :: Packed
pattern PACKED_B = Packed 1

pattern PACKED_C :: Packed
pattern PACKED_C = Packed 2

newtype EnumA = EnumA
  { un_EnumA :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum EnumA where

  type CEnumZ EnumA = FC.CUInt

  toCEnum = EnumA

  fromCEnum = un_EnumA

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "A_FOO"), (1, Data.List.NonEmpty.singleton "A_BAR")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minDeclaredValue = A_FOO

  maxDeclaredValue = A_BAR

instance Show EnumA where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumA where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern A_FOO :: EnumA
pattern A_FOO = EnumA 0

pattern A_BAR :: EnumA
pattern A_BAR = EnumA 1

newtype EnumB = EnumB
  { un_EnumB :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum EnumB where

  type CEnumZ EnumB = FC.CUInt

  toCEnum = EnumB

  fromCEnum = un_EnumB

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "B_FOO"), (1, Data.List.NonEmpty.singleton "B_BAR")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumB"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumB"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumB where

  minDeclaredValue = B_FOO

  maxDeclaredValue = B_BAR

instance Show EnumB where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumB where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern B_FOO :: EnumB
pattern B_FOO = EnumB 0

pattern B_BAR :: EnumB
pattern B_BAR = EnumB 1

newtype EnumC = EnumC
  { un_EnumC :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum EnumC where

  type CEnumZ EnumC = FC.CUInt

  toCEnum = EnumC

  fromCEnum = un_EnumC

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "C_FOO"), (1, Data.List.NonEmpty.singleton "C_BAR")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumC"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumC"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumC where

  minDeclaredValue = C_FOO

  maxDeclaredValue = C_BAR

instance Show EnumC where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumC where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern C_FOO :: EnumC
pattern C_FOO = EnumC 0

pattern C_BAR :: EnumC
pattern C_BAR = EnumC 1

newtype EnumD = EnumD
  { un_EnumD :: FC.CUInt
  }
  deriving stock (Eq, Ord)

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

instance HsBindgen.Runtime.CEnum.CEnum EnumD where

  type CEnumZ EnumD = FC.CUInt

  toCEnum = EnumD

  fromCEnum = un_EnumD

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "D_FOO"), (1, Data.List.NonEmpty.singleton "D_BAR")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumD"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumD"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumD where

  minDeclaredValue = D_FOO

  maxDeclaredValue = D_BAR

instance Show EnumD where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumD where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern D_FOO :: EnumD
pattern D_FOO = EnumD 0

pattern D_BAR :: EnumD
pattern D_BAR = EnumD 1

newtype EnumD_t = EnumD_t
  { un_EnumD_t :: EnumD
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable)
