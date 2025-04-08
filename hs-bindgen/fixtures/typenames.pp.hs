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
import qualified HsBindgen.Runtime.CEnum.Sequential
import Prelude ((<*>), Bounded, Enum, Eq, Floating, Fractional, Int, Num, Ord, Read, Real, RealFloat, RealFrac, Show, pure)

newtype Foo = Foo
  { un_Foo :: FC.CUInt
  }

instance F.Storable Foo where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo un_Foo2 -> F.pokeByteOff ptr0 (0 :: Int) un_Foo2

deriving stock instance Show Foo

deriving stock instance Read Foo

deriving stock instance Eq Foo

deriving stock instance Ord Foo

instance HsBindgen.Runtime.CEnum.Sequential.SequentialCEnum Foo where

  type SequentialCEnumZ Foo = FC.CUInt

  toSequentialCEnum = Foo

  fromSequentialCEnum = un_Foo

  sequentialCEnumMin = \_ -> 0

  sequentialCEnumMax = \_ -> 1

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Foo instance Bounded Foo

deriving via HsBindgen.Runtime.CEnum.Sequential.SeqCEnum Foo instance Enum Foo

pattern FOO1 :: Foo
pattern FOO1 = Foo 0

pattern FOO2 :: Foo
pattern FOO2 = Foo 1

newtype Foo = Foo
  { un_Foo :: FC.CDouble
  }

deriving newtype instance F.Storable Foo

deriving stock instance Eq Foo

deriving stock instance Ord Foo

deriving stock instance Read Foo

deriving stock instance Show Foo

deriving newtype instance Enum Foo

deriving newtype instance Floating Foo

deriving newtype instance Fractional Foo

deriving newtype instance Num Foo

deriving newtype instance Real Foo

deriving newtype instance RealFloat Foo

deriving newtype instance RealFrac Foo
