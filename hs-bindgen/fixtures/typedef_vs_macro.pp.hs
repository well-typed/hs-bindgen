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
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

newtype M1 = M1
  { unM1 :: FC.CInt
  }

deriving newtype instance F.Storable M1

deriving stock instance Eq M1

deriving stock instance Ord M1

deriving stock instance Read M1

deriving stock instance Show M1

deriving newtype instance Enum M1

deriving newtype instance Ix.Ix M1

deriving newtype instance Bounded M1

deriving newtype instance Bits.Bits M1

deriving newtype instance FiniteBits M1

deriving newtype instance Integral M1

deriving newtype instance Num M1

deriving newtype instance Real M1

newtype M2 = M2
  { unM2 :: FC.CChar
  }

deriving newtype instance F.Storable M2

deriving stock instance Eq M2

deriving stock instance Ord M2

deriving stock instance Read M2

deriving stock instance Show M2

deriving newtype instance Enum M2

deriving newtype instance Ix.Ix M2

deriving newtype instance Bounded M2

deriving newtype instance Bits.Bits M2

deriving newtype instance FiniteBits M2

deriving newtype instance Integral M2

deriving newtype instance Num M2

deriving newtype instance Real M2

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

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure ExampleStruct
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          ExampleStruct exampleStruct_t12 exampleStruct_t23 exampleStruct_m14 exampleStruct_m25 ->
               F.pokeByteOff ptr0 (0 :: Int) exampleStruct_t12
            >> F.pokeByteOff ptr0 (4 :: Int) exampleStruct_t23
            >> F.pokeByteOff ptr0 (8 :: Int) exampleStruct_m14
            >> F.pokeByteOff ptr0 (12 :: Int) exampleStruct_m25

deriving stock instance Show ExampleStruct

deriving stock instance Eq ExampleStruct
