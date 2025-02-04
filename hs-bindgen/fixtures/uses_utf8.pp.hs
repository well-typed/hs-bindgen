{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Enum, Eq, Int, Ord, Read, Show, pure)

newtype MyEnum = MyEnum
  { unMyEnum :: FC.CUInt
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
          MyEnum unMyEnum2 -> F.pokeByteOff ptr0 (0 :: Int) unMyEnum2

deriving stock instance Show MyEnum

deriving stock instance Read MyEnum

deriving stock instance Eq MyEnum

deriving stock instance Ord MyEnum

deriving newtype instance Enum MyEnum

pattern Say你好 :: MyEnum
pattern Say你好 = MyEnum 0

pattern Say拜拜 :: MyEnum
pattern Say拜拜 = MyEnum 1
