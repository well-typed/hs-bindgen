{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Data.Bits (FiniteBits)
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ConstantArray
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

newtype T1 = T1
  { un_T1 :: FC.CInt
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
  { un_T2 :: FC.CChar
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

newtype M1 = M1
  { un_M1 :: FC.CInt
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
  { un_M2 :: FC.CChar
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

newtype M3 = M3
  { un_M3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }

deriving newtype instance F.Storable M3

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

newtype Uint64_t = Uint64_t
  { un_Uint64_t :: FC.CInt
  }

deriving newtype instance F.Storable Uint64_t

deriving stock instance Eq Uint64_t

deriving stock instance Ord Uint64_t

deriving stock instance Read Uint64_t

deriving stock instance Show Uint64_t

deriving newtype instance Enum Uint64_t

deriving newtype instance Ix.Ix Uint64_t

deriving newtype instance Bounded Uint64_t

deriving newtype instance Bits.Bits Uint64_t

deriving newtype instance FiniteBits Uint64_t

deriving newtype instance Integral Uint64_t

deriving newtype instance Num Uint64_t

deriving newtype instance Real Uint64_t

data Foo = Foo
  { foo_a :: F.Ptr Uint64_t
  }

instance F.Storable Foo where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_a2 -> F.pokeByteOff ptr0 (0 :: Int) foo_a2

deriving stock instance Show Foo

deriving stock instance Eq Foo
