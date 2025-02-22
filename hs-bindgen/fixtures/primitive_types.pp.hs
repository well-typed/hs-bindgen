{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Primitive = Primitive
  { primitive_c :: FC.CChar
  , primitive_sc :: FC.CSChar
  , primitive_uc :: FC.CSChar
  , primitive_s :: FC.CShort
  , primitive_si :: FC.CShort
  , primitive_ss :: FC.CShort
  , primitive_ssi :: FC.CShort
  , primitive_us :: FC.CUShort
  , primitive_usi :: FC.CUShort
  , primitive_i :: FC.CInt
  , primitive_s2 :: FC.CInt
  , primitive_si2 :: FC.CInt
  , primitive_u :: FC.CUInt
  , primitive_ui :: FC.CUInt
  , primitive_l :: FC.CLong
  , primitive_li :: FC.CLong
  , primitive_sl :: FC.CLong
  , primitive_sli :: FC.CLong
  , primitive_ul :: FC.CULong
  , primitive_uli :: FC.CULong
  , primitive_ll :: FC.CLLong
  , primitive_lli :: FC.CLLong
  , primitive_sll :: FC.CLLong
  , primitive_slli :: FC.CLLong
  , primitive_ull :: FC.CULLong
  , primitive_ulli :: FC.CULLong
  , primitive_f :: FC.CFloat
  , primitive_d :: FC.CDouble
  , primitive_ld :: FC.CDouble
  }

instance F.Storable Primitive where

  sizeOf = \_ -> (176 :: Int)

  alignment = \_ -> (16 :: Int)

  peek =
    \ptr0 ->
          pure Primitive
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (6 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)
      <*> F.peekByteOff ptr0 (10 :: Int)
      <*> F.peekByteOff ptr0 (12 :: Int)
      <*> F.peekByteOff ptr0 (14 :: Int)
      <*> F.peekByteOff ptr0 (16 :: Int)
      <*> F.peekByteOff ptr0 (20 :: Int)
      <*> F.peekByteOff ptr0 (24 :: Int)
      <*> F.peekByteOff ptr0 (28 :: Int)
      <*> F.peekByteOff ptr0 (32 :: Int)
      <*> F.peekByteOff ptr0 (40 :: Int)
      <*> F.peekByteOff ptr0 (48 :: Int)
      <*> F.peekByteOff ptr0 (56 :: Int)
      <*> F.peekByteOff ptr0 (64 :: Int)
      <*> F.peekByteOff ptr0 (72 :: Int)
      <*> F.peekByteOff ptr0 (80 :: Int)
      <*> F.peekByteOff ptr0 (88 :: Int)
      <*> F.peekByteOff ptr0 (96 :: Int)
      <*> F.peekByteOff ptr0 (104 :: Int)
      <*> F.peekByteOff ptr0 (112 :: Int)
      <*> F.peekByteOff ptr0 (120 :: Int)
      <*> F.peekByteOff ptr0 (128 :: Int)
      <*> F.peekByteOff ptr0 (136 :: Int)
      <*> F.peekByteOff ptr0 (144 :: Int)
      <*> F.peekByteOff ptr0 (160 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Primitive
            primitive_c2
            primitive_sc3
            primitive_uc4
            primitive_s5
            primitive_si6
            primitive_ss7
            primitive_ssi8
            primitive_us9
            primitive_usi10
            primitive_i11
            primitive_s212
            primitive_si213
            primitive_u14
            primitive_ui15
            primitive_l16
            primitive_li17
            primitive_sl18
            primitive_sli19
            primitive_ul20
            primitive_uli21
            primitive_ll22
            primitive_lli23
            primitive_sll24
            primitive_slli25
            primitive_ull26
            primitive_ulli27
            primitive_f28
            primitive_d29
            primitive_ld30 ->
                 F.pokeByteOff ptr0 (0 :: Int) primitive_c2
              >> F.pokeByteOff ptr0 (1 :: Int) primitive_sc3
              >> F.pokeByteOff ptr0 (2 :: Int) primitive_uc4
              >> F.pokeByteOff ptr0 (4 :: Int) primitive_s5
              >> F.pokeByteOff ptr0 (6 :: Int) primitive_si6
              >> F.pokeByteOff ptr0 (8 :: Int) primitive_ss7
              >> F.pokeByteOff ptr0 (10 :: Int) primitive_ssi8
              >> F.pokeByteOff ptr0 (12 :: Int) primitive_us9
              >> F.pokeByteOff ptr0 (14 :: Int) primitive_usi10
              >> F.pokeByteOff ptr0 (16 :: Int) primitive_i11
              >> F.pokeByteOff ptr0 (20 :: Int) primitive_s212
              >> F.pokeByteOff ptr0 (24 :: Int) primitive_si213
              >> F.pokeByteOff ptr0 (28 :: Int) primitive_u14
              >> F.pokeByteOff ptr0 (32 :: Int) primitive_ui15
              >> F.pokeByteOff ptr0 (40 :: Int) primitive_l16
              >> F.pokeByteOff ptr0 (48 :: Int) primitive_li17
              >> F.pokeByteOff ptr0 (56 :: Int) primitive_sl18
              >> F.pokeByteOff ptr0 (64 :: Int) primitive_sli19
              >> F.pokeByteOff ptr0 (72 :: Int) primitive_ul20
              >> F.pokeByteOff ptr0 (80 :: Int) primitive_uli21
              >> F.pokeByteOff ptr0 (88 :: Int) primitive_ll22
              >> F.pokeByteOff ptr0 (96 :: Int) primitive_lli23
              >> F.pokeByteOff ptr0 (104 :: Int) primitive_sll24
              >> F.pokeByteOff ptr0 (112 :: Int) primitive_slli25
              >> F.pokeByteOff ptr0 (120 :: Int) primitive_ull26
              >> F.pokeByteOff ptr0 (128 :: Int) primitive_ulli27
              >> F.pokeByteOff ptr0 (136 :: Int) primitive_f28
              >> F.pokeByteOff ptr0 (144 :: Int) primitive_d29
              >> F.pokeByteOff ptr0 (160 :: Int) primitive_ld30

deriving stock instance Show Primitive

deriving stock instance Eq Primitive
