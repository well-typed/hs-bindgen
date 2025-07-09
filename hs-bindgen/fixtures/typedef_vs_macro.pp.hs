{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype T2 = T2
  { un_T2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype M1 = M1
  { un_M1 :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype M2 = M2
  { un_M2 :: FC.CChar
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

newtype M3 = M3
  { un_M3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

newtype M4 = M4
  { un_M4 :: F.Ptr FC.CInt
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

data ExampleStruct = ExampleStruct
  { exampleStruct_t1 :: T1
  , exampleStruct_t2 :: T2
  , exampleStruct_m1 :: M1
  , exampleStruct_m2 :: M2
  }
  deriving stock (Eq, Show)

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
          ExampleStruct
            exampleStruct_t12
            exampleStruct_t23
            exampleStruct_m14
            exampleStruct_m25 ->
                 F.pokeByteOff ptr0 (0 :: Int) exampleStruct_t12
              >> F.pokeByteOff ptr0 (4 :: Int) exampleStruct_t23
              >> F.pokeByteOff ptr0 (8 :: Int) exampleStruct_m14
              >> F.pokeByteOff ptr0 (12 :: Int) exampleStruct_m25

newtype Uint64_t = Uint64_t
  { un_Uint64_t :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

data Foo = Foo
  { foo_a :: F.Ptr Uint64_t
  }
  deriving stock (Eq, Show)

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
