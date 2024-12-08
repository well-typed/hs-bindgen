{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data Foo = Foo
  { foo_i :: FC.CInt
  , foo_c :: FC.CChar
  }

instance F.Storable Foo where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Foo
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Foo foo_i2 foo_c3 ->
               F.pokeByteOff ptr0 0 foo_i2
            >> F.pokeByteOff ptr0 4 foo_c3

data Bar = Bar
  { bar_foo1 :: Foo
  , bar_foo2 :: Foo
  }

instance F.Storable Bar where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_foo12 bar_foo23 ->
               F.pokeByteOff ptr0 0 bar_foo12
            >> F.pokeByteOff ptr0 8 bar_foo23
