{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Show, pure)

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

  sizeOf = \_ -> 176

  alignment = \_ -> 16

  peek =
    \ptr0 ->
          pure Primitive
      <*> F.peekByteOff ptr0 0
      <*> F.peekByteOff ptr0 1
      <*> F.peekByteOff ptr0 2
      <*> F.peekByteOff ptr0 4
      <*> F.peekByteOff ptr0 6
      <*> F.peekByteOff ptr0 8
      <*> F.peekByteOff ptr0 10
      <*> F.peekByteOff ptr0 12
      <*> F.peekByteOff ptr0 14
      <*> F.peekByteOff ptr0 16
      <*> F.peekByteOff ptr0 20
      <*> F.peekByteOff ptr0 24
      <*> F.peekByteOff ptr0 28
      <*> F.peekByteOff ptr0 32
      <*> F.peekByteOff ptr0 40
      <*> F.peekByteOff ptr0 48
      <*> F.peekByteOff ptr0 56
      <*> F.peekByteOff ptr0 64
      <*> F.peekByteOff ptr0 72
      <*> F.peekByteOff ptr0 80
      <*> F.peekByteOff ptr0 88
      <*> F.peekByteOff ptr0 96
      <*> F.peekByteOff ptr0 104
      <*> F.peekByteOff ptr0 112
      <*> F.peekByteOff ptr0 120
      <*> F.peekByteOff ptr0 128
      <*> F.peekByteOff ptr0 136
      <*> F.peekByteOff ptr0 144
      <*> F.peekByteOff ptr0 160

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
                 F.pokeByteOff ptr0 0 primitive_c2
              >> F.pokeByteOff ptr0 1 primitive_sc3
              >> F.pokeByteOff ptr0 2 primitive_uc4
              >> F.pokeByteOff ptr0 4 primitive_s5
              >> F.pokeByteOff ptr0 6 primitive_si6
              >> F.pokeByteOff ptr0 8 primitive_ss7
              >> F.pokeByteOff ptr0 10 primitive_ssi8
              >> F.pokeByteOff ptr0 12 primitive_us9
              >> F.pokeByteOff ptr0 14 primitive_usi10
              >> F.pokeByteOff ptr0 16 primitive_i11
              >> F.pokeByteOff ptr0 20 primitive_s212
              >> F.pokeByteOff ptr0 24 primitive_si213
              >> F.pokeByteOff ptr0 28 primitive_u14
              >> F.pokeByteOff ptr0 32 primitive_ui15
              >> F.pokeByteOff ptr0 40 primitive_l16
              >> F.pokeByteOff ptr0 48 primitive_li17
              >> F.pokeByteOff ptr0 56 primitive_sl18
              >> F.pokeByteOff ptr0 64 primitive_sli19
              >> F.pokeByteOff ptr0 72 primitive_ul20
              >> F.pokeByteOff ptr0 80 primitive_uli21
              >> F.pokeByteOff ptr0 88 primitive_ll22
              >> F.pokeByteOff ptr0 96 primitive_lli23
              >> F.pokeByteOff ptr0 104 primitive_sll24
              >> F.pokeByteOff ptr0 112 primitive_slli25
              >> F.pokeByteOff ptr0 120 primitive_ull26
              >> F.pokeByteOff ptr0 128 primitive_ulli27
              >> F.pokeByteOff ptr0 136 primitive_f28
              >> F.pokeByteOff ptr0 144 primitive_d29
              >> F.pokeByteOff ptr0 160 primitive_ld30

deriving stock instance Show Primitive

deriving stock instance Eq Primitive
