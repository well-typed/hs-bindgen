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

{-| __C declaration:__ @Dim2@

    __defined at:__ @unions.h:1:8@

    __exported by:__ @unions.h@
-}
data Dim2 = Dim2
  { dim2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @unions.h:2:9@

         __exported by:__ @unions.h@
    -}
  , dim2_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @unions.h:3:9@

         __exported by:__ @unions.h@
    -}
  }
  deriving stock (Eq, Show)

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

{-| __C declaration:__ @Dim3@

    __defined at:__ @unions.h:6:8@

    __exported by:__ @unions.h@
-}
data Dim3 = Dim3
  { dim3_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @unions.h:7:9@

         __exported by:__ @unions.h@
    -}
  , dim3_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @unions.h:8:9@

         __exported by:__ @unions.h@
    -}
  , dim3_z :: FC.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @unions.h:9:9@

         __exported by:__ @unions.h@
    -}
  }
  deriving stock (Eq, Show)

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

{-| __C declaration:__ @DimPayload@

    __defined at:__ @unions.h:12:7@

    __exported by:__ @unions.h@
-}
newtype DimPayload = DimPayload
  { un_DimPayload :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 4 instance F.Storable DimPayload

{-|

  __See:__ 'set_dimPayload_dim2'

__C declaration:__ @dim2@

__defined at:__ @unions.h:13:17@

__exported by:__ @unions.h@
-}
get_dimPayload_dim2 :: DimPayload -> Dim2
get_dimPayload_dim2 =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim2'

-}
set_dimPayload_dim2 :: Dim2 -> DimPayload
set_dimPayload_dim2 =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_dimPayload_dim3'

__C declaration:__ @dim3@

__defined at:__ @unions.h:14:17@

__exported by:__ @unions.h@
-}
get_dimPayload_dim3 :: DimPayload -> Dim2
get_dimPayload_dim3 =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim3'

-}
set_dimPayload_dim3 :: Dim2 -> DimPayload
set_dimPayload_dim3 =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @Dim@

    __defined at:__ @unions.h:17:8@

    __exported by:__ @unions.h@
-}
data Dim = Dim
  { dim_tag :: FC.CInt
    {- ^ __C declaration:__ @tag@

         __defined at:__ @unions.h:18:9@

         __exported by:__ @unions.h@
    -}
  , dim_payload :: DimPayload
    {- ^ __C declaration:__ @payload@

         __defined at:__ @unions.h:19:22@

         __exported by:__ @unions.h@
    -}
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

{-| __C declaration:__ @DimPayloadB@

    __defined at:__ @unions.h:23:15@

    __exported by:__ @unions.h@
-}
newtype DimPayloadB = DimPayloadB
  { un_DimPayloadB :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 8) 4 instance F.Storable DimPayloadB

{-|

  __See:__ 'set_dimPayloadB_dim2'

__C declaration:__ @dim2@

__defined at:__ @unions.h:24:17@

__exported by:__ @unions.h@
-}
get_dimPayloadB_dim2 :: DimPayloadB -> Dim2
get_dimPayloadB_dim2 =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim2'

-}
set_dimPayloadB_dim2 :: Dim2 -> DimPayloadB
set_dimPayloadB_dim2 =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_dimPayloadB_dim3'

__C declaration:__ @dim3@

__defined at:__ @unions.h:25:17@

__exported by:__ @unions.h@
-}
get_dimPayloadB_dim3 :: DimPayloadB -> Dim2
get_dimPayloadB_dim3 =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim3'

-}
set_dimPayloadB_dim3 :: Dim2 -> DimPayloadB
set_dimPayloadB_dim3 =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-| __C declaration:__ @DimB@

    __defined at:__ @unions.h:28:8@

    __exported by:__ @unions.h@
-}
data DimB = DimB
  { dimB_tag :: FC.CInt
    {- ^ __C declaration:__ @tag@

         __defined at:__ @unions.h:29:9@

         __exported by:__ @unions.h@
    -}
  , dimB_payload :: DimPayloadB
    {- ^ __C declaration:__ @payload@

         __defined at:__ @unions.h:30:17@

         __exported by:__ @unions.h@
    -}
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

{-| __C declaration:__ @AnonA_xy@

    __defined at:__ @unions.h:35:5@

    __exported by:__ @unions.h@
-}
data AnonA_xy = AnonA_xy
  { anonA_xy_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @unions.h:35:21@

         __exported by:__ @unions.h@
    -}
  , anonA_xy_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @unions.h:35:31@

         __exported by:__ @unions.h@
    -}
  }
  deriving stock (Eq, Show)

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

{-| __C declaration:__ @AnonA_polar@

    __defined at:__ @unions.h:36:5@

    __exported by:__ @unions.h@
-}
data AnonA_polar = AnonA_polar
  { anonA_polar_r :: FC.CDouble
    {- ^ __C declaration:__ @r@

         __defined at:__ @unions.h:36:21@

         __exported by:__ @unions.h@
    -}
  , anonA_polar_p :: FC.CDouble
    {- ^ __C declaration:__ @p@

         __defined at:__ @unions.h:36:31@

         __exported by:__ @unions.h@
    -}
  }
  deriving stock (Eq, Show)

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

{-| __C declaration:__ @AnonA@

    __defined at:__ @unions.h:34:7@

    __exported by:__ @unions.h@
-}
newtype AnonA = AnonA
  { un_AnonA :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 16) 8 instance F.Storable AnonA

{-|

  __See:__ 'set_anonA_xy'

__C declaration:__ @xy@

__defined at:__ @unions.h:35:36@

__exported by:__ @unions.h@
-}
get_anonA_xy :: AnonA -> AnonA_xy
get_anonA_xy =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_anonA_xy'

-}
set_anonA_xy :: AnonA_xy -> AnonA
set_anonA_xy =
  HsBindgen.Runtime.ByteArray.setUnionPayload

{-|

  __See:__ 'set_anonA_polar'

__C declaration:__ @polar@

__defined at:__ @unions.h:36:36@

__exported by:__ @unions.h@
-}
get_anonA_polar :: AnonA -> AnonA_polar
get_anonA_polar =
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_anonA_polar'

-}
set_anonA_polar :: AnonA_polar -> AnonA
set_anonA_polar =
  HsBindgen.Runtime.ByteArray.setUnionPayload
