{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype CFirst = MkCFirst
  { unCFirst :: FC.CUInt
  }

instance F.Storable CFirst where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCFirst
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCFirst unCFirst2 -> F.pokeByteOff x0 0 unCFirst2

newtype CSecond = MkCSecond
  { unCSecond :: FC.CInt
  }

instance F.Storable CSecond where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCSecond
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCSecond unCSecond2 -> F.pokeByteOff x0 0 unCSecond2

newtype CSame = MkCSame
  { unCSame :: FC.CUInt
  }

instance F.Storable CSame where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCSame
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCSame unCSame2 -> F.pokeByteOff x0 0 unCSame2

newtype CPackad = MkCPackad
  { unCPackad :: FC.CSChar
  }

instance F.Storable CPackad where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \x0 ->
          pure MkCPackad
      <*> F.peekByteOff x0 0

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCPackad unCPackad2 -> F.pokeByteOff x0 0 unCPackad2
