{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

newtype CS1T = MkCS1T
  { unCS1T :: CStruct'0020S1
  }

deriving newtype instance F.Storable CS1T

data CS1 = MkCS1
  { cS1_a :: FC.CInt
  }

instance F.Storable CS1 where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS1
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS1 cS1_a2 -> F.pokeByteOff ptr0 0 cS1_a2

data CS2 = MkCS2
  { cS2_a :: FC.CInt
  }

instance F.Storable CS2 where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCS2
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCS2 cS2_a2 -> F.pokeByteOff ptr0 0 cS2_a2
