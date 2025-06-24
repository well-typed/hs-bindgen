{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Structs where

import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.Bitfield
import qualified HsBindgen.Runtime.CAPI as CAPI
import qualified HsBindgen.Runtime.FlexibleArrayMember
import qualified HsBindgen.Runtime.Prelude
import Prelude ((<*>), (>>), Eq, IO, Int, Show, pure)

$(CAPI.addCSource "#include \"structs.h\"\nstruct surname *Structs_surname_alloc (char arg1[]) { return surname_alloc(arg1); }\nvoid Structs_surname_free (struct surname *arg1) { surname_free(arg1); }\nstruct square *Structs_create_square (double arg1) { return create_square(arg1); }\n")

data Door = Door
  { door_height :: FC.CFloat
  , door_width :: FC.CFloat
  }

instance F.Storable Door where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Door
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Door door_height2 door_width3 ->
               F.pokeByteOff ptr0 (0 :: Int) door_height2
            >> F.pokeByteOff ptr0 (4 :: Int) door_width3

deriving stock instance Show Door

deriving stock instance Eq Door

data Room = Room
  { room_door1 :: Door
  , room_door2 :: Door
  }

instance F.Storable Room where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Room
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Room room_door12 room_door23 ->
               F.pokeByteOff ptr0 (0 :: Int) room_door12
            >> F.pokeByteOff ptr0 (8 :: Int) room_door23

deriving stock instance Show Room

deriving stock instance Eq Room

data Aula2_door = Aula2_door
  { aula2_door_height :: FC.CFloat
  , aula2_door_width :: FC.CFloat
  }

instance F.Storable Aula2_door where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Aula2_door
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Aula2_door aula2_door_height2 aula2_door_width3 ->
               F.pokeByteOff ptr0 (0 :: Int) aula2_door_height2
            >> F.pokeByteOff ptr0 (4 :: Int) aula2_door_width3

deriving stock instance Show Aula2_door

deriving stock instance Eq Aula2_door

data Aula2 = Aula2
  { aula2_door :: Aula2_door
  , aula2_n_doors :: FC.CInt
  }

instance F.Storable Aula2 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Aula2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Aula2 aula2_door2 aula2_n_doors3 ->
               F.pokeByteOff ptr0 (0 :: Int) aula2_door2
            >> F.pokeByteOff ptr0 (8 :: Int) aula2_n_doors3

deriving stock instance Show Aula2

deriving stock instance Eq Aula2

data Aula_setup = Aula_setup
  { aula_setup_window_id :: FC.CChar
  , aula_setup_tilt :: FC.CInt
  , aula_setup_close_blinds :: FC.CInt
  , aula_setup_projector_id :: FC.CChar
  , aula_setup_power_mode :: FC.CInt
  }

instance F.Storable Aula_setup where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Aula_setup
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (8 :: Int) (1 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (9 :: Int) (1 :: Int)
      <*> F.peekByteOff ptr0 (2 :: Int)
      <*> HsBindgen.Runtime.Bitfield.peekBitOffWidth ptr0 (24 :: Int) (2 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Aula_setup
            aula_setup_window_id2
            aula_setup_tilt3
            aula_setup_close_blinds4
            aula_setup_projector_id5
            aula_setup_power_mode6 ->
                 F.pokeByteOff ptr0 (0 :: Int) aula_setup_window_id2
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (8 :: Int) (1 :: Int) aula_setup_tilt3
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (9 :: Int) (1 :: Int) aula_setup_close_blinds4
              >> F.pokeByteOff ptr0 (2 :: Int) aula_setup_projector_id5
              >> HsBindgen.Runtime.Bitfield.pokeBitOffWidth ptr0 (24 :: Int) (2 :: Int) aula_setup_power_mode6

deriving stock instance Show Aula_setup

deriving stock instance Eq Aula_setup

data Surname = Surname
  { surname_len :: HsBindgen.Runtime.Prelude.CSize
  }

instance F.Storable Surname where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Surname
      <*> F.peekByteOff ptr0 (0 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Surname surname_len2 ->
            F.pokeByteOff ptr0 (0 :: Int) surname_len2

deriving stock instance Show Surname

deriving stock instance Eq Surname

instance HsBindgen.Runtime.FlexibleArrayMember.HasFlexibleArrayMember FC.CChar Surname where

  flexibleArrayMemberOffset = \_ty0 -> 8

foreign import ccall safe "Structs_surname_alloc" surname_alloc :: (F.Ptr FC.CChar) -> IO (F.Ptr Surname)

foreign import ccall safe "Structs_surname_free" surname_free :: (F.Ptr Surname) -> IO ()

data Square

foreign import ccall safe "Structs_create_square" create_square :: FC.CDouble -> IO (F.Ptr Square)
