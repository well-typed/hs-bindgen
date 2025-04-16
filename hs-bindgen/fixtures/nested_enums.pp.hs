{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Data.Map.Strict
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum
import Prelude ((<*>), Eq, Int, Ord, Read, Show, pure, show)
import qualified Prelude as P

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
    \_ -> Data.Map.Strict.fromList [(0, pure "VALA_1"), (1, pure "VALA_2")]

  rangeIsSequential = \_ -> P.Just (0, 1)

instance Show EnumA where

  show = HsBindgen.Runtime.CEnum.showCEnum "EnumA"

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

deriving stock instance Read ExB_fieldB1

instance HsBindgen.Runtime.CEnum.CEnum ExB_fieldB1 where

  type CEnumZ ExB_fieldB1 = FC.CUInt

  fromCEnumZ = ExB_fieldB1

  toCEnumZ = un_ExB_fieldB1

  declaredValues =
    \_ -> Data.Map.Strict.fromList [(0, pure "VALB_1"), (1, pure "VALB_2")]

  rangeIsSequential = \_ -> P.Just (0, 1)

instance Show ExB_fieldB1 where

  show = HsBindgen.Runtime.CEnum.showCEnum "ExB_fieldB1"

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
