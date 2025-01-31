{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Show, pure)

newtype BOOL = BOOL
  { unBOOL :: FC.CBool
  }

data Bools1 = Bools1
  { bools1_x :: FC.CBool
  , bools1_y :: FC.CBool
  }

instance F.Storable Bools1 where

  sizeOf = \_ -> 2

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure Bools1
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools1 bools1_x2 bools1_y3 ->
               F.pokeByteOff ptr0 0 bools1_x2
            >> F.pokeByteOff ptr0 1 bools1_y3

deriving stock instance Show Bools1

data Bools2 = Bools2
  { bools2_x :: FC.CBool
  , bools2_y :: FC.CBool
  }

instance F.Storable Bools2 where

  sizeOf = \_ -> 2

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure Bools2
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools2 bools2_x2 bools2_y3 ->
               F.pokeByteOff ptr0 0 bools2_x2
            >> F.pokeByteOff ptr0 1 bools2_y3

deriving stock instance Show Bools2

data Bools3 = Bools3
  { bools3_x :: BOOL
  , bools3_y :: BOOL
  }

instance F.Storable Bools3 where

  sizeOf = \_ -> 2

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure Bools3
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bools3 bools3_x2 bools3_y3 ->
               F.pokeByteOff ptr0 0 bools3_x2
            >> F.pokeByteOff ptr0 1 bools3_y3

deriving stock instance Show Bools3
