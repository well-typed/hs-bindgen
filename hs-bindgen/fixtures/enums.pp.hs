{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

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

pattern MkCFIRST1 :: CFirst
pattern MkCFIRST1 = MkCFirst 0

pattern MkCFIRST2 :: CFirst
pattern MkCFIRST2 = MkCFirst 1

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

pattern MkCSECONDA :: CSecond
pattern MkCSECONDA = MkCSecond -1

pattern MkCSECONDB :: CSecond
pattern MkCSECONDB = MkCSecond 0

pattern MkCSECONDC :: CSecond
pattern MkCSECONDC = MkCSecond 1

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

pattern MkCSAMEA :: CSame
pattern MkCSAMEA = MkCSame 1

pattern MkCSAMEB :: CSame
pattern MkCSAMEB = MkCSame 1

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

pattern MkCPACKEDA :: CPackad
pattern MkCPACKEDA = MkCPackad 0

pattern MkCPACKEDB :: CPackad
pattern MkCPACKEDB = MkCPackad 1

pattern MkCPACKEDC :: CPackad
pattern MkCPACKEDC = MkCPackad 2

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

pattern MkCAFOO :: CEnumA
pattern MkCAFOO = MkCEnumA 0

pattern MkCABAR :: CEnumA
pattern MkCABAR = MkCEnumA 1

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

pattern MkCBFOO :: CEnumB
pattern MkCBFOO = MkCEnumB 0

pattern MkCBBAR :: CEnumB
pattern MkCBBAR = MkCEnumB 1

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

pattern MkCCFOO :: CEnumC
pattern MkCCFOO = MkCEnumC 0

pattern MkCCBAR :: CEnumC
pattern MkCCBAR = MkCEnumC 1

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

pattern MkCDFOO :: CEnumD
pattern MkCDFOO = MkCEnumD 0

pattern MkCDBAR :: CEnumD
pattern MkCDBAR = MkCEnumD 1

newtype CEnumDT = MkCEnumDT
  { unCEnumDT :: CEnumD
  }

deriving newtype instance F.Storable CEnumDT
