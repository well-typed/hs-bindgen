{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Bitfield
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @flags@

    __defined at:__ @bitfields.h:1:8@

    __exported by:__ @bitfields.h@
-}
data Flags = Flags
  { flags_fieldX :: FC.CChar
    {- ^ __C declaration:__ @fieldX@

         __defined at:__ @bitfields.h:2:10@

         __exported by:__ @bitfields.h@
    -}
  , flags_flagA :: FC.CInt
    {- ^ __C declaration:__ @flagA@

         __defined at:__ @bitfields.h:3:9@

         __exported by:__ @bitfields.h@
    -}
  , flags_flagB :: FC.CInt
    {- ^ __C declaration:__ @flagB@

         __defined at:__ @bitfields.h:4:9@

         __exported by:__ @bitfields.h@
    -}
  , flags_flagC :: FC.CInt
    {- ^ __C declaration:__ @flagC@

         __defined at:__ @bitfields.h:5:9@

         __exported by:__ @bitfields.h@
    -}
  , flags_fieldY :: FC.CChar
    {- ^ __C declaration:__ @fieldY@

         __defined at:__ @bitfields.h:6:10@

         __exported by:__ @bitfields.h@
    -}
  , flags_bits :: FC.CInt
    {- ^ __C declaration:__ @bits@

         __defined at:__ @bitfields.h:7:9@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Flags where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Flags
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (8 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (9 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (10 :: Int) (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (24 :: Int) (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Flags
            flags_fieldX2
            flags_flagA3
            flags_flagB4
            flags_flagC5
            flags_fieldY6
            flags_bits7 ->
                 F.pokeByteOff ptr0 (0 :: Int) flags_fieldX2
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (8 :: Int) (1 :: Int) flags_flagA3
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (9 :: Int) (1 :: Int) flags_flagB4
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (10 :: Int) (1 :: Int) flags_flagC5
              >> F.pokeByteOff ptr0 (2 :: Int) flags_fieldY6
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (24 :: Int) (2 :: Int) flags_bits7

{-| __C declaration:__ @overflow32@

    __defined at:__ @bitfields.h:12:8@

    __exported by:__ @bitfields.h@
-}
data Overflow32 = Overflow32
  { overflow32_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:13:9@

         __exported by:__ @bitfields.h@
    -}
  , overflow32_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:14:9@

         __exported by:__ @bitfields.h@
    -}
  , overflow32_z :: FC.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @bitfields.h:15:9@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Overflow32 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Overflow32
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (32 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (64 :: Int) (17 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32 overflow32_x2 overflow32_y3 overflow32_z4 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (17 :: Int) overflow32_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (32 :: Int) (17 :: Int) overflow32_y3
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (64 :: Int) (17 :: Int) overflow32_z4

{-| __C declaration:__ @overflow32b@

    __defined at:__ @bitfields.h:18:8@

    __exported by:__ @bitfields.h@
-}
data Overflow32b = Overflow32b
  { overflow32b_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:19:10@

         __exported by:__ @bitfields.h@
    -}
  , overflow32b_y :: FC.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:20:10@

         __exported by:__ @bitfields.h@
    -}
  , overflow32b_z :: FC.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @bitfields.h:21:10@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Overflow32b where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Overflow32b
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (17 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (34 :: Int) (17 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32b overflow32b_x2 overflow32b_y3 overflow32b_z4 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (17 :: Int) overflow32b_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (17 :: Int) (17 :: Int) overflow32b_y3
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (34 :: Int) (17 :: Int) overflow32b_z4

{-| __C declaration:__ @overflow32c@

    __defined at:__ @bitfields.h:24:8@

    __exported by:__ @bitfields.h@
-}
data Overflow32c = Overflow32c
  { overflow32c_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:25:10@

         __exported by:__ @bitfields.h@
    -}
  , overflow32c_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:26:10@

         __exported by:__ @bitfields.h@
    -}
  , overflow32c_z :: FC.CLong
    {- ^ __C declaration:__ @z@

         __defined at:__ @bitfields.h:27:10@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Overflow32c where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Overflow32c
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (32 :: Int) (17 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (64 :: Int) (17 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow32c overflow32c_x2 overflow32c_y3 overflow32c_z4 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (17 :: Int) overflow32c_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (32 :: Int) (17 :: Int) overflow32c_y3
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (64 :: Int) (17 :: Int) overflow32c_z4

{-| __C declaration:__ @overflow64@

    __defined at:__ @bitfields.h:30:8@

    __exported by:__ @bitfields.h@
-}
data Overflow64 = Overflow64
  { overflow64_x :: FC.CLong
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:31:10@

         __exported by:__ @bitfields.h@
    -}
  , overflow64_y :: FC.CLong
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:32:10@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable Overflow64 where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Overflow64
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (33 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (64 :: Int) (33 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Overflow64 overflow64_x2 overflow64_y3 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (33 :: Int) overflow64_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (64 :: Int) (33 :: Int) overflow64_y3

{-| __C declaration:__ @alignA@

    __defined at:__ @bitfields.h:36:8@

    __exported by:__ @bitfields.h@
-}
data AlignA = AlignA
  { alignA_x :: FC.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:37:16@

         __exported by:__ @bitfields.h@
    -}
  , alignA_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:38:6@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable AlignA where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure AlignA
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (1 :: Int) (10 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignA alignA_x2 alignA_y3 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (1 :: Int) alignA_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (1 :: Int) (10 :: Int) alignA_y3

{-| __C declaration:__ @alignB@

    __defined at:__ @bitfields.h:41:8@

    __exported by:__ @bitfields.h@
-}
data AlignB = AlignB
  { alignB_x :: FC.CUChar
    {- ^ __C declaration:__ @x@

         __defined at:__ @bitfields.h:42:16@

         __exported by:__ @bitfields.h@
    -}
  , alignB_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @bitfields.h:43:6@

         __exported by:__ @bitfields.h@
    -}
  }
  deriving stock (Eq, Show)

instance F.Storable AlignB where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure AlignB
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (0 :: Int) (7 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (32 :: Int) (31 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AlignB alignB_x2 alignB_y3 ->
               HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (0 :: Int) (7 :: Int) alignB_x2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (32 :: Int) (31 :: Int) alignB_y3
