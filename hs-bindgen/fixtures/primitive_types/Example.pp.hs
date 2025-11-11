{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @primitive@

    __defined at:__ @primitive_types.h:1:8@

    __exported by:__ @primitive_types.h@
-}
data Primitive = Primitive
  { primitive_c :: FC.CChar
    {- ^ __C declaration:__ @c@

         __defined at:__ @primitive_types.h:2:10@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_sc :: FC.CSChar
    {- ^ __C declaration:__ @sc@

         __defined at:__ @primitive_types.h:3:17@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_uc :: FC.CUChar
    {- ^ __C declaration:__ @uc@

         __defined at:__ @primitive_types.h:4:19@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_s :: FC.CShort
    {- ^ __C declaration:__ @s@

         __defined at:__ @primitive_types.h:6:11@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_si :: FC.CShort
    {- ^ __C declaration:__ @si@

         __defined at:__ @primitive_types.h:7:15@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ss :: FC.CShort
    {- ^ __C declaration:__ @ss@

         __defined at:__ @primitive_types.h:8:18@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ssi :: FC.CShort
    {- ^ __C declaration:__ @ssi@

         __defined at:__ @primitive_types.h:9:22@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_us :: FC.CUShort
    {- ^ __C declaration:__ @us@

         __defined at:__ @primitive_types.h:11:20@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_usi :: FC.CUShort
    {- ^ __C declaration:__ @usi@

         __defined at:__ @primitive_types.h:12:24@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_i :: FC.CInt
    {- ^ __C declaration:__ @i@

         __defined at:__ @primitive_types.h:14:9@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_s2 :: FC.CInt
    {- ^ __C declaration:__ @s2@

         __defined at:__ @primitive_types.h:15:12@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_si2 :: FC.CInt
    {- ^ __C declaration:__ @si2@

         __defined at:__ @primitive_types.h:16:16@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_u :: FC.CUInt
    {- ^ __C declaration:__ @u@

         __defined at:__ @primitive_types.h:18:14@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ui :: FC.CUInt
    {- ^ __C declaration:__ @ui@

         __defined at:__ @primitive_types.h:19:18@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_l :: FC.CLong
    {- ^ __C declaration:__ @l@

         __defined at:__ @primitive_types.h:21:10@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_li :: FC.CLong
    {- ^ __C declaration:__ @li@

         __defined at:__ @primitive_types.h:22:14@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_sl :: FC.CLong
    {- ^ __C declaration:__ @sl@

         __defined at:__ @primitive_types.h:23:17@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_sli :: FC.CLong
    {- ^ __C declaration:__ @sli@

         __defined at:__ @primitive_types.h:24:21@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ul :: FC.CULong
    {- ^ __C declaration:__ @ul@

         __defined at:__ @primitive_types.h:26:19@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_uli :: FC.CULong
    {- ^ __C declaration:__ @uli@

         __defined at:__ @primitive_types.h:27:23@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ll :: FC.CLLong
    {- ^ __C declaration:__ @ll@

         __defined at:__ @primitive_types.h:29:15@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_lli :: FC.CLLong
    {- ^ __C declaration:__ @lli@

         __defined at:__ @primitive_types.h:30:19@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_sll :: FC.CLLong
    {- ^ __C declaration:__ @sll@

         __defined at:__ @primitive_types.h:31:22@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_slli :: FC.CLLong
    {- ^ __C declaration:__ @slli@

         __defined at:__ @primitive_types.h:32:26@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ull :: FC.CULLong
    {- ^ __C declaration:__ @ull@

         __defined at:__ @primitive_types.h:34:24@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_ulli :: FC.CULLong
    {- ^ __C declaration:__ @ulli@

         __defined at:__ @primitive_types.h:35:28@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_f :: FC.CFloat
    {- ^ __C declaration:__ @f@

         __defined at:__ @primitive_types.h:37:11@

         __exported by:__ @primitive_types.h@
    -}
  , primitive_d :: FC.CDouble
    {- ^ __C declaration:__ @d@

         __defined at:__ @primitive_types.h:38:12@

         __exported by:__ @primitive_types.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Primitive where

  sizeOf = \_ -> (152 :: Int)

  alignment = \_ -> (8 :: Int)

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
            primitive_d29 ->
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
