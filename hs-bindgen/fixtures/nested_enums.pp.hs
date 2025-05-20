{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.List.NonEmpty
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, showsPrec)
import qualified Text.Read

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

instance HsBindgen.Runtime.CEnum.CEnum EnumA where

  type CEnumZ EnumA = FC.CUInt

  toCEnum = EnumA

  fromCEnum = un_EnumA

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "VALA_1"), (1, Data.List.NonEmpty.singleton "VALA_2")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "EnumA"

  readPrecUndeclared = HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "EnumA"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum EnumA where

  minDeclaredValue = VALA_1

  maxDeclaredValue = VALA_2

instance Show EnumA where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read EnumA where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern VALA_1 :: EnumA
pattern VALA_1 = EnumA 0

pattern VALA_2 :: EnumA
pattern VALA_2 = EnumA 1

data ExA = ExA
  { exA_fieldA1 :: EnumA
  }

instance F.Storable ExA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExA
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExA exA_fieldA12 -> F.pokeByteOff ptr0 (0 :: Int) exA_fieldA12

deriving stock instance Show ExA

deriving stock instance Eq ExA

newtype ExB_fieldB1 = ExB_fieldB1
  { un_ExB_fieldB1 :: FC.CUInt
  }

instance F.Storable ExB_fieldB1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB_fieldB1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB_fieldB1 un_ExB_fieldB12 -> F.pokeByteOff ptr0 (0 :: Int) un_ExB_fieldB12

deriving stock instance Eq ExB_fieldB1

deriving stock instance Ord ExB_fieldB1

instance HsBindgen.Runtime.CEnum.CEnum ExB_fieldB1 where

  type CEnumZ ExB_fieldB1 = FC.CUInt

  toCEnum = ExB_fieldB1

  fromCEnum = un_ExB_fieldB1

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "VALB_1"), (1, Data.List.NonEmpty.singleton "VALB_2")]

  showsUndeclared = HsBindgen.Runtime.CEnum.showsWrappedUndeclared "ExB_fieldB1"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "ExB_fieldB1"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum ExB_fieldB1 where

  minDeclaredValue = VALB_1

  maxDeclaredValue = VALB_2

instance Show ExB_fieldB1 where

  showsPrec = HsBindgen.Runtime.CEnum.showsCEnum

instance Read ExB_fieldB1 where

  readPrec = HsBindgen.Runtime.CEnum.readPrecCEnum

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

pattern VALB_1 :: ExB_fieldB1
pattern VALB_1 = ExB_fieldB1 0

pattern VALB_2 :: ExB_fieldB1
pattern VALB_2 = ExB_fieldB1 1

data ExB = ExB
  { exB_fieldB1 :: ExB_fieldB1
  }

instance F.Storable ExB where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExB
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExB exB_fieldB12 -> F.pokeByteOff ptr0 (0 :: Int) exB_fieldB12

deriving stock instance Show ExB

deriving stock instance Eq ExB
