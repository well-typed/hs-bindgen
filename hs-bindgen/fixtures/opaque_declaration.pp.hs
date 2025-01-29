{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import Prelude ((<*>), (>>), pure, return)

data Foo

data Bar = Bar
  { bar_ptrA :: F.Ptr Foo
  , bar_ptrB :: F.Ptr Bar
  }

instance F.Storable Bar where

  sizeOf = \_ -> 16

  alignment = \_ -> 8

  peek =
    \ptr0 ->
          pure Bar
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 8

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Bar bar_ptrA2 bar_ptrB3 ->
               F.pokeByteOff ptr0 0 bar_ptrA2
            >> F.pokeByteOff ptr0 8 bar_ptrB3

data Baz = Baz
  {}

instance F.Storable Baz where

  sizeOf = \_ -> 0

  alignment = \_ -> 1

  peek = \ptr0 -> pure Baz

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Baz -> return ()

data Quu
