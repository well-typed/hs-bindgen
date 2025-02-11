{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Bitfield
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Flags = Flags
  { flags_fieldX :: FC.CChar
  , flags_flagA :: FC.CInt
  , flags_flagB :: FC.CInt
  , flags_flagC :: FC.CInt
  , flags_fieldY :: FC.CChar
  , flags_bits :: FC.CInt
  }

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
          Flags flags_fieldX2 flags_flagA3 flags_flagB4 flags_flagC5 flags_fieldY6 flags_bits7 ->
               F.pokeByteOff ptr0 (0 :: Int) flags_fieldX2
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (8 :: Int) (1 :: Int) flags_flagA3
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (9 :: Int) (1 :: Int) flags_flagB4
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (10 :: Int) (1 :: Int) flags_flagC5
            >> F.pokeByteOff ptr0 (2 :: Int) flags_fieldY6
            >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (24 :: Int) (2 :: Int) flags_bits7

deriving stock instance Show Flags

deriving stock instance Eq Flags

data Overflow32 = Overflow32
  { overflow32_x :: FC.CInt
  , overflow32_y :: FC.CInt
  , overflow32_z :: FC.CInt
  }

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

deriving stock instance Show Overflow32

deriving stock instance Eq Overflow32

data Overflow32b = Overflow32b
  { overflow32b_x :: FC.CLong
  , overflow32b_y :: FC.CLong
  , overflow32b_z :: FC.CLong
  }

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

deriving stock instance Show Overflow32b

deriving stock instance Eq Overflow32b

data Overflow32c = Overflow32c
  { overflow32c_x :: FC.CLong
  , overflow32c_y :: FC.CInt
  , overflow32c_z :: FC.CLong
  }

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

deriving stock instance Show Overflow32c

deriving stock instance Eq Overflow32c

data Overflow64 = Overflow64
  { overflow64_x :: FC.CLong
  , overflow64_y :: FC.CLong
  }

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

deriving stock instance Show Overflow64

deriving stock instance Eq Overflow64

data AlignA = AlignA
  { alignA_x :: FC.CSChar
  , alignA_y :: FC.CInt
  }

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

deriving stock instance Show AlignA

deriving stock instance Eq AlignA

data AlignB = AlignB
  { alignB_x :: FC.CSChar
  , alignB_y :: FC.CInt
  }

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

deriving stock instance Show AlignB

deriving stock instance Eq AlignB
