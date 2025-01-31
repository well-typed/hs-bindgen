{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show, pure)

newtype M1 = M1
  { unM1 :: FC.CInt
  }

newtype M2 = M2
  { unM2 :: FC.CChar
  }

newtype T1 = T1
  { unT1 :: FC.CInt
  }

deriving newtype instance F.Storable T1

deriving stock instance Eq T1

deriving stock instance Ord T1

deriving stock instance Read T1

deriving stock instance Show T1

deriving newtype instance Enum T1

deriving newtype instance Ix.Ix T1

deriving newtype instance Bounded T1

deriving newtype instance Bits.Bits T1

deriving newtype instance FiniteBits T1

deriving newtype instance Integral T1

deriving newtype instance Num T1

deriving newtype instance Real T1

newtype T2 = T2
  { unT2 :: FC.CChar
  }

deriving newtype instance F.Storable T2

deriving stock instance Eq T2

deriving stock instance Ord T2

deriving stock instance Read T2

deriving stock instance Show T2

deriving newtype instance Enum T2

deriving newtype instance Ix.Ix T2

deriving newtype instance Bounded T2

deriving newtype instance Bits.Bits T2

deriving newtype instance FiniteBits T2

deriving newtype instance Integral T2

deriving newtype instance Num T2

deriving newtype instance Real T2

data ExampleStruct = ExampleStruct
  { exampleStruct_t1 :: T1
  , exampleStruct_t2 :: T2
  , exampleStruct_m1 :: M1
  , exampleStruct_m2 :: M2
  }

instance F.Storable ExampleStruct where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure ExampleStruct
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8
      <*> F.peekByteOff ptr0 12

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct exampleStruct_t12 exampleStruct_t23 exampleStruct_m14 exampleStruct_m25 ->
               F.pokeByteOff ptr0 0 exampleStruct_t12
            >> F.pokeByteOff ptr0 4 exampleStruct_t23
            >> F.pokeByteOff ptr0 8 exampleStruct_m14
            >> F.pokeByteOff ptr0 12 exampleStruct_m25

deriving stock instance Show ExampleStruct

deriving stock instance Eq ExampleStruct
