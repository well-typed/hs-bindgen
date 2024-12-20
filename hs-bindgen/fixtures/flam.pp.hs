{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), pure)

data CPascal = MkCPascal
  { cPascal_len :: FC.CInt
  }

instance F.Storable CPascal where

  sizeOf = \_ -> 4

  alignment = \_ -> 4

  peek =
    \ptr0 ->
          pure MkCPascal
      <*> F.peekByteOff ptr0 0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCPascal cPascal_len2 -> F.pokeByteOff ptr0 0 cPascal_len2
