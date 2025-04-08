{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.CEnum.Sequential
import Prelude ((<*>), Bounded, Enum, Eq, Int, Ord, Read, Show, pure)

newtype MyEnum = MyEnum
  { un_MyEnum :: FC.CUInt
  }

instance F.Storable MyEnum where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure MyEnum
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MyEnum un_MyEnum2 -> F.pokeByteOff ptr0 (0 :: Int) un_MyEnum2

deriving stock instance Show MyEnum

deriving stock instance Read MyEnum

deriving stock instance Eq MyEnum

deriving stock instance Ord MyEnum

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum MyEnum where

  type SequentialCEnumZ MyEnum = FC.CUInt

  toSequentialCEnum = MyEnum

  fromSequentialCEnum = un_MyEnum

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum MyEnum instance Bounded MyEnum

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum MyEnum instance Enum MyEnum

pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
