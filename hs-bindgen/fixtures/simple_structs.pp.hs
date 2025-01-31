{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Show, pure)

data S1 = S1
  { s1_a :: FC.CInt
  , s1_b :: FC.CChar
  }

instance F.Storable S1 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               F.pokeByteOff ptr0 0 s1_a2
            >> F.pokeByteOff ptr0 4 s1_b3

deriving stock instance Show S1

deriving stock instance Eq S1

data S2 = S2
  { s2_a :: FC.CChar
  , s2_b :: FC.CInt
  , s2_c :: FC.CFloat
  }

instance F.Storable S2 where

  sizeOf = \_ -> 12

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 s2_b3 s2_c4 ->
               F.pokeByteOff ptr0 0 s2_a2
            >> F.pokeByteOff ptr0 4 s2_b3
            >> F.pokeByteOff ptr0 8 s2_c4

deriving stock instance Show S2

deriving stock instance Eq S2

newtype S2_t = S2_t
  { unS2_t :: S2
  }

deriving newtype instance F.Storable S2_t

data S3_t = S3_t
  { s3_t_a :: FC.CChar
  }

instance F.Storable S3_t where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure S3_t
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 -> F.pokeByteOff ptr0 0 s3_t_a2

deriving stock instance Show S3_t

deriving stock instance Eq S3_t

data S4 = S4
  { s4_b :: FC.CChar
  , s4_a :: FC.CInt
  , s4_c :: F.Ptr FC.CInt
  }

instance F.Storable S4 where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure S4
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               F.pokeByteOff ptr0 0 s4_b2
            >> F.pokeByteOff ptr0 4 s4_a3
            >> F.pokeByteOff ptr0 8 s4_c4

deriving stock instance Show S4

deriving stock instance Eq S4

data S5 = S5
  { s5_a :: FC.CChar
  , s5_b :: FC.CInt
  }

instance F.Storable S5 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S5
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               F.pokeByteOff ptr0 0 s5_a2
            >> F.pokeByteOff ptr0 4 s5_b3

deriving stock instance Show S5

deriving stock instance Eq S5

data S6 = S6
  { s6_a :: FC.CChar
  , s6_b :: FC.CInt
  }

instance F.Storable S6 where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S6
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               F.pokeByteOff ptr0 0 s6_a2
            >> F.pokeByteOff ptr0 4 s6_b3

deriving stock instance Show S6

deriving stock instance Eq S6
