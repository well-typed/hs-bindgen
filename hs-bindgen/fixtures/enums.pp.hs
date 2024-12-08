{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype First = First
  { unFirst :: FC.CUInt
  }

instance F.Storable First where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure First
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          First unFirst2 -> F.pokeByteOff ptr0 0 unFirst2

pattern FIRST1 :: First
pattern FIRST1 = First 0

pattern FIRST2 :: First
pattern FIRST2 = First 1

newtype Second = Second
  { unSecond :: FC.CInt
  }

instance F.Storable Second where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Second
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Second unSecond2 -> F.pokeByteOff ptr0 0 unSecond2

pattern SECOND_A :: Second
pattern SECOND_A = Second (-1)

pattern SECOND_B :: Second
pattern SECOND_B = Second 0

pattern SECOND_C :: Second
pattern SECOND_C = Second 1

newtype Same = Same
  { unSame :: FC.CUInt
  }

instance F.Storable Same where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure Same
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Same unSame2 -> F.pokeByteOff ptr0 0 unSame2

pattern SAME_A :: Same
pattern SAME_A = Same 1

pattern SAME_B :: Same
pattern SAME_B = Same 1

newtype Packad = Packad
  { unPackad :: FC.CSChar
  }

instance F.Storable Packad where

  sizeOf = \_ -> 1

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure Packad
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Packad unPackad2 -> F.pokeByteOff ptr0 0 unPackad2

pattern PACKED_A :: Packad
pattern PACKED_A = Packad 0

pattern PACKED_B :: Packad
pattern PACKED_B = Packad 1

pattern PACKED_C :: Packad
pattern PACKED_C = Packad 2

newtype EnumA = EnumA
  { unEnumA :: FC.CUInt
  }

instance F.Storable EnumA where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure EnumA
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumA unEnumA2 -> F.pokeByteOff ptr0 0 unEnumA2

pattern A_FOO :: EnumA
pattern A_FOO = EnumA 0

pattern A_BAR :: EnumA
pattern A_BAR = EnumA 1

newtype EnumB = EnumB
  { unEnumB :: FC.CUInt
  }

instance F.Storable EnumB where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure EnumB
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumB unEnumB2 -> F.pokeByteOff ptr0 0 unEnumB2

pattern B_FOO :: EnumB
pattern B_FOO = EnumB 0

pattern B_BAR :: EnumB
pattern B_BAR = EnumB 1

newtype EnumC = EnumC
  { unEnumC :: FC.CUInt
  }

instance F.Storable EnumC where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure EnumC
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumC unEnumC2 -> F.pokeByteOff ptr0 0 unEnumC2

pattern C_FOO :: EnumC
pattern C_FOO = EnumC 0

pattern C_BAR :: EnumC
pattern C_BAR = EnumC 1

newtype EnumD = EnumD
  { unEnumD :: FC.CUInt
  }

instance F.Storable EnumD where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure EnumD
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          EnumD unEnumD2 -> F.pokeByteOff ptr0 0 unEnumD2

pattern D_FOO :: EnumD
pattern D_FOO = EnumD 0

pattern D_BAR :: EnumD
pattern D_BAR = EnumD 1

newtype EnumD_t = EnumD_t
  { unEnumD_t :: EnumD
  }

deriving newtype instance F.Storable EnumD_t
