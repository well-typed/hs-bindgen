{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CX = MkCX
  { cX_foo :: FC.CInt
  , cX_bar :: FC.CInt
  }

instance F.Storable CX where

  sizeOf = \_ -> 8

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCX
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 4

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCX cX_foo2 cX_bar3 ->
               F.pokeByteOff ptr0 0 cX_foo2
            >> F.pokeByteOff ptr0 4 cX_bar3
