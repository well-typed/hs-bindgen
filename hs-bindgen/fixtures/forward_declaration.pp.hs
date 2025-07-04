{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Int, Show, pure)

data S1 = S1
  { s1_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S1 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 -> F.pokeByteOff ptr0 (0 :: Int) s1_a2

newtype S1_t = S1_t
  { un_S1_t :: S1
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

data S2 = S2
  { s2_a :: FC.CInt
  }
  deriving stock (Eq, Show)

instance F.Storable S2 where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 -> F.pokeByteOff ptr0 (0 :: Int) s2_a2
