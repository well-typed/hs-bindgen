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
    \ptr0 ->
          pure MkCFirst
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCFirst unCFirst2 -> F.pokeByteOff ptr0 0 unCFirst2

newtype CSecond = MkCSecond
  { unCSecond :: FC.CInt
  }

instance F.Storable CSecond where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCSecond
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCSecond unCSecond2 -> F.pokeByteOff ptr0 0 unCSecond2

newtype CSame = MkCSame
  { unCSame :: FC.CUInt
  }

instance F.Storable CSame where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCSame
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCSame unCSame2 -> F.pokeByteOff ptr0 0 unCSame2

newtype CPackad = MkCPackad
  { unCPackad :: FC.CSChar
  }

instance F.Storable CPackad where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCPackad
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCPackad unCPackad2 -> F.pokeByteOff ptr0 0 unCPackad2

newtype CEnumA = MkCEnumA
  { unCEnumA :: FC.CUInt
  }

instance F.Storable CEnumA where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCEnumA
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCEnumA unCEnumA2 -> F.pokeByteOff ptr0 0 unCEnumA2

newtype CEnumB = MkCEnumB
  { unCEnumB :: FC.CUInt
  }

instance F.Storable CEnumB where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCEnumB
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCEnumB unCEnumB2 -> F.pokeByteOff ptr0 0 unCEnumB2

newtype CEnumC = MkCEnumC
  { unCEnumC :: FC.CUInt
  }

instance F.Storable CEnumC where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCEnumC
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCEnumC unCEnumC2 -> F.pokeByteOff ptr0 0 unCEnumC2

newtype CEnumD = MkCEnumD
  { unCEnumD :: FC.CUInt
  }

instance F.Storable CEnumD where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCEnumD
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCEnumD unCEnumD2 -> F.pokeByteOff ptr0 0 unCEnumD2

newtype CEnumDT = MkCEnumDT
  { unCEnumDT :: CEnumD
  }

deriving newtype instance F.Storable CEnumDT
