{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Ord, Show, pure)

data S1 = S1
  { s1_a :: FC.CInt
  , s1_b :: FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 s1_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s1_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s1_b3

data S2_t = S2_t
  { s2_t_a :: FC.CChar
  , s2_t_b :: FC.CInt
  , s2_t_c :: FC.CFloat
  }
  deriving stock (Eq, Show)

instance F.Storable S2_t where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2_t
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2_t s2_t_a2 s2_t_b3 s2_t_c4 ->
               F.pokeByteOff ptr0 (0 :: Int) s2_t_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s2_t_b3
            >> F.pokeByteOff ptr0 (8 :: Int) s2_t_c4

data S3_t = S3_t
  { s3_t_a :: FC.CChar
  }
  deriving stock (Eq, Show)

instance F.Storable S3_t where

  sizeOf = \_ -> (1 :: Int)

  alignment = \_ -> (1 :: Int)

  peek =
    \ptr0 ->
          pure S3_t
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S3_t s3_t_a2 -> F.pokeByteOff ptr0 (0 :: Int) s3_t_a2

data S4 = S4
  { s4_b :: FC.CChar
  , s4_a :: FC.CInt
  , s4_c :: F.Ptr FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S4 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure S4
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S4 s4_b2 s4_a3 s4_c4 ->
               F.pokeByteOff ptr0 (0 :: Int) s4_b2
            >> F.pokeByteOff ptr0 (4 :: Int) s4_a3
            >> F.pokeByteOff ptr0 (8 :: Int) s4_c4

data S5 = S5
  { s5_a :: FC.CChar
  , s5_b :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S5 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S5
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S5 s5_a2 s5_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s5_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s5_b3

data S6 = S6
  { s6_a :: FC.CChar
  , s6_b :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S6 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S6
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S6 s6_a2 s6_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s6_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s6_b3

data S7a_Deref = S7a_Deref
  { s7a_Deref_a :: FC.CChar
  , s7a_Deref_b :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S7a_Deref where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S7a_Deref
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7a_Deref s7a_Deref_a2 s7a_Deref_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s7a_Deref_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s7a_Deref_b3

newtype S7a = S7a
  { un_S7a :: F.Ptr S7a_Deref
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)

data S7b_Deref = S7b_Deref
  { s7b_Deref_a :: FC.CChar
  , s7b_Deref_b :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S7b_Deref where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S7b_Deref
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S7b_Deref s7b_Deref_a2 s7b_Deref_b3 ->
               F.pokeByteOff ptr0 (0 :: Int) s7b_Deref_a2
            >> F.pokeByteOff ptr0 (4 :: Int) s7b_Deref_b3

newtype S7b = S7b
  { un_S7b :: F.Ptr (F.Ptr (F.Ptr S7b_Deref))
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype (F.Storable)
