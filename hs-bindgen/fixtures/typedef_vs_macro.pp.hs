{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Show, pure)

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

newtype T2 = T2
  { unT2 :: FC.CChar
  }

deriving newtype instance F.Storable T2

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
