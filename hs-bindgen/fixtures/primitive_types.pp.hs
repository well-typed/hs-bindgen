{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), pure)

data CPrimitive = MkCPrimitive
  { cPrimitive_c :: FC.CChar
  , cPrimitive_sc :: FC.CSChar
  , cPrimitive_uc :: FC.CSChar
  , cPrimitive_s :: FC.CShort
  , cPrimitive_si :: FC.CShort
  , cPrimitive_ss :: FC.CShort
  , cPrimitive_ssi :: FC.CShort
  , cPrimitive_us :: FC.CUShort
  , cPrimitive_usi :: FC.CUShort
  , cPrimitive_i :: FC.CInt
  , cPrimitive_s2 :: FC.CInt
  , cPrimitive_si2 :: FC.CInt
  , cPrimitive_u :: FC.CUInt
  , cPrimitive_ui :: FC.CUInt
  , cPrimitive_l :: FC.CLong
  , cPrimitive_li :: FC.CLong
  , cPrimitive_sl :: FC.CLong
  , cPrimitive_sli :: FC.CLong
  , cPrimitive_ul :: FC.CULong
  , cPrimitive_uli :: FC.CULong
  , cPrimitive_ll :: FC.CLLong
  , cPrimitive_lli :: FC.CLLong
  , cPrimitive_sll :: FC.CLLong
  , cPrimitive_slli :: FC.CLLong
  , cPrimitive_ull :: FC.CULLong
  , cPrimitive_ulli :: FC.CULLong
  , cPrimitive_f :: FC.CFloat
  , cPrimitive_d :: FC.CDouble
  , cPrimitive_ld :: FC.CDouble
  }

instance F.Storable CPrimitive where

  sizeOf = \_ -> 176

  alignment = \_ -> 16

  peek =
    \x0 ->
          pure MkCPrimitive
      <*> F.peekByteOff x0 0
      <*> F.peekByteOff x0 8
      <*> F.peekByteOff x0 16
      <*> F.peekByteOff x0 32
      <*> F.peekByteOff x0 48
      <*> F.peekByteOff x0 64
      <*> F.peekByteOff x0 80
      <*> F.peekByteOff x0 96
      <*> F.peekByteOff x0 112
      <*> F.peekByteOff x0 128
      <*> F.peekByteOff x0 160
      <*> F.peekByteOff x0 192
      <*> F.peekByteOff x0 224
      <*> F.peekByteOff x0 256
      <*> F.peekByteOff x0 320
      <*> F.peekByteOff x0 384
      <*> F.peekByteOff x0 448
      <*> F.peekByteOff x0 512
      <*> F.peekByteOff x0 576
      <*> F.peekByteOff x0 640
      <*> F.peekByteOff x0 704
      <*> F.peekByteOff x0 768
      <*> F.peekByteOff x0 832
      <*> F.peekByteOff x0 896
      <*> F.peekByteOff x0 960
      <*> F.peekByteOff x0 1024
      <*> F.peekByteOff x0 1088
      <*> F.peekByteOff x0 1152
      <*> F.peekByteOff x0 1280

  poke =
    \x0 ->
      \x1 ->
        case x1 of
          MkCPrimitive
            cPrimitive_c2
            cPrimitive_sc3
            cPrimitive_uc4
            cPrimitive_s5
            cPrimitive_si6
            cPrimitive_ss7
            cPrimitive_ssi8
            cPrimitive_us9
            cPrimitive_usi10
            cPrimitive_i11
            cPrimitive_s212
            cPrimitive_si213
            cPrimitive_u14
            cPrimitive_ui15
            cPrimitive_l16
            cPrimitive_li17
            cPrimitive_sl18
            cPrimitive_sli19
            cPrimitive_ul20
            cPrimitive_uli21
            cPrimitive_ll22
            cPrimitive_lli23
            cPrimitive_sll24
            cPrimitive_slli25
            cPrimitive_ull26
            cPrimitive_ulli27
            cPrimitive_f28
            cPrimitive_d29
            cPrimitive_ld30 ->
                 F.pokeByteOff x0 0 cPrimitive_c2
              >> F.pokeByteOff x0 8 cPrimitive_sc3
              >> F.pokeByteOff x0 16 cPrimitive_uc4
              >> F.pokeByteOff x0 32 cPrimitive_s5
              >> F.pokeByteOff x0 48 cPrimitive_si6
              >> F.pokeByteOff x0 64 cPrimitive_ss7
              >> F.pokeByteOff x0 80 cPrimitive_ssi8
              >> F.pokeByteOff x0 96 cPrimitive_us9
              >> F.pokeByteOff x0 112 cPrimitive_usi10
              >> F.pokeByteOff x0 128 cPrimitive_i11
              >> F.pokeByteOff x0 160 cPrimitive_s212
              >> F.pokeByteOff x0 192 cPrimitive_si213
              >> F.pokeByteOff x0 224 cPrimitive_u14
              >> F.pokeByteOff x0 256 cPrimitive_ui15
              >> F.pokeByteOff x0 320 cPrimitive_l16
              >> F.pokeByteOff x0 384 cPrimitive_li17
              >> F.pokeByteOff x0 448 cPrimitive_sl18
              >> F.pokeByteOff x0 512 cPrimitive_sli19
              >> F.pokeByteOff x0 576 cPrimitive_ul20
              >> F.pokeByteOff x0 640 cPrimitive_uli21
              >> F.pokeByteOff x0 704 cPrimitive_ll22
              >> F.pokeByteOff x0 768 cPrimitive_lli23
              >> F.pokeByteOff x0 832 cPrimitive_sll24
              >> F.pokeByteOff x0 896 cPrimitive_slli25
              >> F.pokeByteOff x0 960 cPrimitive_ull26
              >> F.pokeByteOff x0 1024 cPrimitive_ulli27
              >> F.pokeByteOff x0 1088 cPrimitive_f28
              >> F.pokeByteOff x0 1152 cPrimitive_d29
              >> F.pokeByteOff x0 1280 cPrimitive_ld30
