{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example where

import qualified Data.Array.Byte
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified HsBindgen.Runtime.ByteArray
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
  { un_DimPayload :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 4 instance F.Storable DimPayload

get_dimPayload_dim2 :: DimPayload -> Dim2
get_dimPayload_dim2 = HsBindgen.Runtime.ByteArray.getUnionPayload

set_dimPayload_dim2 :: Dim2 -> DimPayload
set_dimPayload_dim2 = HsBindgen.Runtime.ByteArray.setUnionPayload

get_dimPayload_dim3 :: DimPayload -> Dim2
get_dimPayload_dim3 = HsBindgen.Runtime.ByteArray.getUnionPayload

set_dimPayload_dim3 :: Dim2 -> DimPayload
set_dimPayload_dim3 = HsBindgen.Runtime.ByteArray.setUnionPayload

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

newtype DimPayloadB = DimPayloadB
  { un_DimPayloadB :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 4 instance F.Storable DimPayloadB

get_dimPayloadB_dim2 :: DimPayloadB -> Dim2
get_dimPayloadB_dim2 = HsBindgen.Runtime.ByteArray.getUnionPayload

set_dimPayloadB_dim2 :: Dim2 -> DimPayloadB
set_dimPayloadB_dim2 = HsBindgen.Runtime.ByteArray.setUnionPayload

get_dimPayloadB_dim3 :: DimPayloadB -> Dim2
get_dimPayloadB_dim3 = HsBindgen.Runtime.ByteArray.getUnionPayload

set_dimPayloadB_dim3 :: Dim2 -> DimPayloadB
set_dimPayloadB_dim3 = HsBindgen.Runtime.ByteArray.setUnionPayload

data DimB = DimB
  { dimB_tag :: FC.CInt
  , dimB_payload :: DimPayloadB
  }

instance F.Storable DimB where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure DimB
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (4 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          DimB dimB_tag2 dimB_payload3 ->
               F.pokeByteOff ptr0 (0 :: Int) dimB_tag2
            >> F.pokeByteOff ptr0 (4 :: Int) dimB_payload3

data AnonA_polar = AnonA_polar
  { anonA_polar_r :: FC.CDouble
  , anonA_polar_p :: FC.CDouble
  }

instance F.Storable AnonA_polar where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure AnonA_polar
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_polar anonA_polar_r2 anonA_polar_p3 ->
               F.pokeByteOff ptr0 (0 :: Int) anonA_polar_r2
            >> F.pokeByteOff ptr0 (8 :: Int) anonA_polar_p3

deriving stock instance Show AnonA_polar

deriving stock instance Eq AnonA_polar

data AnonA_xy = AnonA_xy
  { anonA_xy_x :: FC.CDouble
  , anonA_xy_y :: FC.CDouble
  }

instance F.Storable AnonA_xy where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure AnonA_xy
      <*> F.peekByteOff ptr0 (0 :: Int)
      <*> F.peekByteOff ptr0 (8 :: Int)

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_xy anonA_xy_x2 anonA_xy_y3 ->
               F.pokeByteOff ptr0 (0 :: Int) anonA_xy_x2
            >> F.pokeByteOff ptr0 (8 :: Int) anonA_xy_y3

deriving stock instance Show AnonA_xy

deriving stock instance Eq AnonA_xy

newtype AnonA = AnonA
  { un_AnonA :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 16) 8 instance F.Storable AnonA

get_anonA_xy :: AnonA -> AnonA_xy
get_anonA_xy = HsBindgen.Runtime.ByteArray.getUnionPayload

set_anonA_xy :: AnonA_xy -> AnonA
set_anonA_xy = HsBindgen.Runtime.ByteArray.setUnionPayload

get_anonA_polar :: AnonA -> AnonA_polar
get_anonA_polar = HsBindgen.Runtime.ByteArray.getUnionPayload

set_anonA_polar :: AnonA_polar -> AnonA
set_anonA_polar = HsBindgen.Runtime.ByteArray.setUnionPayload
