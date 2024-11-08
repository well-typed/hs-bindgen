{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

newtype CT1 = MkCT1
  { unCT1 :: FC.CInt
  }

newtype CT2 = MkCT2
  { unCT2 :: FC.CChar
  }

data CExampleStruct = MkCExampleStruct
  { cExampleStruct_t1 :: CT1
  , cExampleStruct_t2 :: CT2
  , cExampleStruct_m1 :: CM1
  , cExampleStruct_m2 :: CM2
  }

instance F.Storable CExampleStruct where

  sizeOf = \_ -> 16

  alignment = \_ -> 4

  peek =
    \x0 ->
          pure MkCExampleStruct
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 32
      <*> F.peekByteOff x0 64
      <*> F.peekByteOff x0 96

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCExampleStruct cExampleStruct_t12 cExampleStruct_t23 cExampleStruct_m14 cExampleStruct_m25 ->
               F.pokeByteOff x0 0 cExampleStruct_t12
            >> F.pokeByteOff x0 32 cExampleStruct_t23
            >> F.pokeByteOff x0 64 cExampleStruct_m14
            >> F.pokeByteOff x0 96 cExampleStruct_m25
