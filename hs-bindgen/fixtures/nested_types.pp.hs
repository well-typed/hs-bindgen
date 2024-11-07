{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CFoo = MkCFoo
  { cFoo_i :: FC.CInt
  , cFoo_c :: FC.CChar
  }

instance F.Storable CFoo where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCFoo
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 32

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCFoo cFoo_i2 cFoo_c3 ->
               F.pokeByteOff x0 0 cFoo_i2
            >> F.pokeByteOff x0 32 cFoo_c3

data CBar = MkCBar
  { cBar_foo1 :: CStruct'0020foo
  , cBar_foo2 :: CStruct'0020foo
  }

instance F.Storable CBar where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCBar
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 64

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCBar cBar_foo12 cBar_foo23 ->
               F.pokeByteOff x0 0 cBar_foo12
            >> F.pokeByteOff x0 64 cBar_foo23
