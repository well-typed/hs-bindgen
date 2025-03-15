{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.SizedByteArray
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

data Dim2 = Dim2
  { dim2_x :: FC.CInt
  , dim2_y :: FC.CInt
  }

instance F.Storable Dim2 where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Dim2
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim2 dim2_x2 dim2_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) dim2_x2
            >> F.pokeByteOff ptr0 (4 :: Int) dim2_y3

deriving stock instance Show Dim2

deriving stock instance Eq Dim2

data Dim3 = Dim3
  { dim3_x :: FC.CInt
  , dim3_y :: FC.CInt
  , dim3_z :: FC.CInt
  }

instance F.Storable Dim3 where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Dim3
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim3 dim3_x2 dim3_y3 dim3_z4 ->
               F.pokeByteOff ptr0 (0 :: Int) dim3_x2
            >> F.pokeByteOff ptr0 (4 :: Int) dim3_y3
            >> F.pokeByteOff ptr0 (8 :: Int) dim3_z4

deriving stock instance Show Dim3

deriving stock instance Eq Dim3

newtype DimPayload = DimPayload
  { unDimPayload :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 4 instance F.Storable DimPayload

data Dim = Dim
  { dim_tag :: FC.CInt
  , dim_payload :: DimPayload
  }

instance F.Storable Dim where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Dim
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim dim_tag2 dim_payload3 ->
               F.pokeByteOff ptr0 (0 :: Int) dim_tag2
            >> F.pokeByteOff ptr0 (4 :: Int) dim_payload3

deriving stock instance Show Dim

deriving stock instance Eq Dim
