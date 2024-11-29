{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import Data.Void (Void)
import qualified Foreign as F
import Prelude ((<*>), (>>), pure)

data CBools1 = MkCBools1
  { cBools1_x :: Void
  , cBools1_y :: Void
  }

instance F.Storable CBools1 where

  sizeOf = \_ -> 2

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCBools1
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCBools1 cBools1_x2 cBools1_y3 ->
               F.pokeByteOff ptr0 0 cBools1_x2
            >> F.pokeByteOff ptr0 1 cBools1_y3

data CBools2 = MkCBools2
  { cBools2_x :: CBool'
  , cBools2_y :: CBool'
  }

instance F.Storable CBools2 where

  sizeOf = \_ -> 2

  alignment = \_ -> 1

  peek =
    \ptr0 ->
          pure MkCBools2
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          MkCBools2 cBools2_x2 cBools2_y3 ->
               F.pokeByteOff ptr0 0 cBools2_x2
            >> F.pokeByteOff ptr0 1 cBools2_y3
