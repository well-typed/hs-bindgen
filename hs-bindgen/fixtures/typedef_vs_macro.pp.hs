{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

newtype CM1 = MkCM1
  { unCM1 :: FC.CInt
  }

newtype CM2 = MkCM2
  { unCM2 :: FC.CChar
  }

newtype CT1 = MkCT1
  { unCT1 :: FC.CInt
  }

deriving newtype instance F.Storable CT1

newtype CT2 = MkCT2
  { unCT2 :: FC.CChar
  }

deriving newtype instance F.Storable CT2

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
      <*> F.peekByteOff x0 4
      <*> F.peekByteOff x0 8
      <*> F.peekByteOff x0 12

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCExampleStruct cExampleStruct_t12 cExampleStruct_t23 cExampleStruct_m14 cExampleStruct_m25 ->
               F.pokeByteOff x0 0 cExampleStruct_t12
            >> F.pokeByteOff x0 4 cExampleStruct_t23
            >> F.pokeByteOff x0 8 cExampleStruct_m14
            >> F.pokeByteOff x0 12 cExampleStruct_m25
