{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), Eq, IO, Show, pure)

foreign import capi safe "weird01.h func" func :: (F.Ptr Bar) -> IO ()

data Foo = Foo
  { foo_z :: FC.CInt
  }

instance F.Storable Foo where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_z2 -> F.pokeByteOff ptr0 0 foo_z2

deriving stock instance Show Foo

deriving stock instance Eq Foo

data Bar = Bar
  { bar_x :: FC.CInt
  }

instance F.Storable Bar where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_x2 -> F.pokeByteOff ptr0 0 bar_x2

deriving stock instance Show Bar

deriving stock instance Eq Bar
