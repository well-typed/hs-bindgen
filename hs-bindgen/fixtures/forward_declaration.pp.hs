{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, Show, pure)

data S1 = S1
  { s1_a :: FC.CInt
  }

instance F.Storable S1 where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S1
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S1 s1_a2 -> F.pokeByteOff ptr0 0 s1_a2

deriving stock instance Show S1

deriving stock instance Eq S1

newtype S1_t = S1_t
  { unS1_t :: S1
  }

deriving newtype instance F.Storable S1_t

data S2 = S2
  { s2_a :: FC.CInt
  }

instance F.Storable S2 where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure S2
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          S2 s2_a2 -> F.pokeByteOff ptr0 0 s2_a2

deriving stock instance Show S2

deriving stock instance Eq S2
