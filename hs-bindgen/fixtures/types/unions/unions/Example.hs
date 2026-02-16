{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct Dim2@

    __defined at:__ @types\/unions\/unions.h 1:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim2 = Dim2
  { dim2_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 2:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim2_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 3:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Dim2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim2 where

  readRaw =
    \ptr0 ->
          pure Dim2
      <*> HasCField.readRaw (RIP.Proxy @"dim2_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dim2_y") ptr0

instance Marshal.WriteRaw Dim2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim2 dim2_x2 dim2_y3 ->
               HasCField.writeRaw (RIP.Proxy @"dim2_x") ptr0 dim2_x2
            >> HasCField.writeRaw (RIP.Proxy @"dim2_y") ptr0 dim2_y3

deriving via Marshal.EquivStorable Dim2 instance RIP.Storable Dim2

instance HasCField.HasCField Dim2 "dim2_x" where

  type CFieldType Dim2 "dim2_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim2_x" (RIP.Ptr Dim2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim2_x")

instance HasCField.HasCField Dim2 "dim2_y" where

  type CFieldType Dim2 "dim2_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim2_y" (RIP.Ptr Dim2) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim2_y")

{-| __C declaration:__ @struct Dim3@

    __defined at:__ @types\/unions\/unions.h 6:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim3 = Dim3
  { dim3_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 7:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 8:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_z :: RIP.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/unions\/unions.h 9:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Dim3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim3 where

  readRaw =
    \ptr0 ->
          pure Dim3
      <*> HasCField.readRaw (RIP.Proxy @"dim3_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dim3_y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dim3_z") ptr0

instance Marshal.WriteRaw Dim3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim3 dim3_x2 dim3_y3 dim3_z4 ->
               HasCField.writeRaw (RIP.Proxy @"dim3_x") ptr0 dim3_x2
            >> HasCField.writeRaw (RIP.Proxy @"dim3_y") ptr0 dim3_y3
            >> HasCField.writeRaw (RIP.Proxy @"dim3_z") ptr0 dim3_z4

deriving via Marshal.EquivStorable Dim3 instance RIP.Storable Dim3

instance HasCField.HasCField Dim3 "dim3_x" where

  type CFieldType Dim3 "dim3_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim3_x" (RIP.Ptr Dim3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim3_x")

instance HasCField.HasCField Dim3 "dim3_y" where

  type CFieldType Dim3 "dim3_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim3_y" (RIP.Ptr Dim3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim3_y")

instance HasCField.HasCField Dim3 "dim3_z" where

  type CFieldType Dim3 "dim3_z" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim3_z" (RIP.Ptr Dim3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim3_z")

{-| __C declaration:__ @union DimPayload@

    __defined at:__ @types\/unions\/unions.h 12:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayload = DimPayload
  { unwrapDimPayload :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.StaticSize DimPayload

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.ReadRaw DimPayload

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.WriteRaw DimPayload

deriving via Marshal.EquivStorable DimPayload instance RIP.Storable DimPayload

{-|

  __See:__ 'set_dimPayload_dim2'

__C declaration:__ @dim2@

__defined at:__ @types\/unions\/unions.h 13:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayload_dim2 ::
     DimPayload
  -> Dim2
get_dimPayload_dim2 = RIP.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim2'

-}
set_dimPayload_dim2 ::
     Dim2
  -> DimPayload
set_dimPayload_dim2 = RIP.setUnionPayload

{-|

  __See:__ 'set_dimPayload_dim3'

__C declaration:__ @dim3@

__defined at:__ @types\/unions\/unions.h 14:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayload_dim3 ::
     DimPayload
  -> Dim2
get_dimPayload_dim3 = RIP.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim3'

-}
set_dimPayload_dim3 ::
     Dim2
  -> DimPayload
set_dimPayload_dim3 = RIP.setUnionPayload

instance HasCField.HasCField DimPayload "dimPayload_dim2" where

  type CFieldType DimPayload "dimPayload_dim2" = Dim2

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Dim2
         ) => RIP.HasField "dimPayload_dim2" (RIP.Ptr DimPayload) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dimPayload_dim2")

instance HasCField.HasCField DimPayload "dimPayload_dim3" where

  type CFieldType DimPayload "dimPayload_dim3" = Dim2

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Dim2
         ) => RIP.HasField "dimPayload_dim3" (RIP.Ptr DimPayload) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dimPayload_dim3")

{-| __C declaration:__ @struct Dim@

    __defined at:__ @types\/unions\/unions.h 17:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim = Dim
  { dim_tag :: RIP.CInt
    {- ^ __C declaration:__ @tag@

         __defined at:__ @types\/unions\/unions.h 18:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim_payload :: DimPayload
    {- ^ __C declaration:__ @payload@

         __defined at:__ @types\/unions\/unions.h 19:22@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize Dim where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim where

  readRaw =
    \ptr0 ->
          pure Dim
      <*> HasCField.readRaw (RIP.Proxy @"dim_tag") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dim_payload") ptr0

instance Marshal.WriteRaw Dim where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim dim_tag2 dim_payload3 ->
               HasCField.writeRaw (RIP.Proxy @"dim_tag") ptr0 dim_tag2
            >> HasCField.writeRaw (RIP.Proxy @"dim_payload") ptr0 dim_payload3

deriving via Marshal.EquivStorable Dim instance RIP.Storable Dim

instance HasCField.HasCField Dim "dim_tag" where

  type CFieldType Dim "dim_tag" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dim_tag" (RIP.Ptr Dim) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dim_tag")

instance HasCField.HasCField Dim "dim_payload" where

  type CFieldType Dim "dim_payload" = DimPayload

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) DimPayload
         ) => RIP.HasField "dim_payload" (RIP.Ptr Dim) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dim_payload")

{-| __C declaration:__ @union DimPayloadB@

    __defined at:__ @types\/unions\/unions.h 23:15@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayloadB = DimPayloadB
  { unwrapDimPayloadB :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.StaticSize DimPayloadB

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.ReadRaw DimPayloadB

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.WriteRaw DimPayloadB

deriving via Marshal.EquivStorable DimPayloadB instance RIP.Storable DimPayloadB

{-|

  __See:__ 'set_dimPayloadB_dim2'

__C declaration:__ @dim2@

__defined at:__ @types\/unions\/unions.h 24:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayloadB_dim2 ::
     DimPayloadB
  -> Dim2
get_dimPayloadB_dim2 = RIP.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim2'

-}
set_dimPayloadB_dim2 ::
     Dim2
  -> DimPayloadB
set_dimPayloadB_dim2 = RIP.setUnionPayload

{-|

  __See:__ 'set_dimPayloadB_dim3'

__C declaration:__ @dim3@

__defined at:__ @types\/unions\/unions.h 25:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayloadB_dim3 ::
     DimPayloadB
  -> Dim2
get_dimPayloadB_dim3 = RIP.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim3'

-}
set_dimPayloadB_dim3 ::
     Dim2
  -> DimPayloadB
set_dimPayloadB_dim3 = RIP.setUnionPayload

instance HasCField.HasCField DimPayloadB "dimPayloadB_dim2" where

  type CFieldType DimPayloadB "dimPayloadB_dim2" = Dim2

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Dim2
         ) => RIP.HasField "dimPayloadB_dim2" (RIP.Ptr DimPayloadB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dimPayloadB_dim2")

instance HasCField.HasCField DimPayloadB "dimPayloadB_dim3" where

  type CFieldType DimPayloadB "dimPayloadB_dim3" = Dim2

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Dim2
         ) => RIP.HasField "dimPayloadB_dim3" (RIP.Ptr DimPayloadB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dimPayloadB_dim3")

{-| __C declaration:__ @struct DimB@

    __defined at:__ @types\/unions\/unions.h 28:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data DimB = DimB
  { dimB_tag :: RIP.CInt
    {- ^ __C declaration:__ @tag@

         __defined at:__ @types\/unions\/unions.h 29:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dimB_payload :: DimPayloadB
    {- ^ __C declaration:__ @payload@

         __defined at:__ @types\/unions\/unions.h 30:17@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (RIP.Generic)

instance Marshal.StaticSize DimB where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw DimB where

  readRaw =
    \ptr0 ->
          pure DimB
      <*> HasCField.readRaw (RIP.Proxy @"dimB_tag") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"dimB_payload") ptr0

instance Marshal.WriteRaw DimB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DimB dimB_tag2 dimB_payload3 ->
               HasCField.writeRaw (RIP.Proxy @"dimB_tag") ptr0 dimB_tag2
            >> HasCField.writeRaw (RIP.Proxy @"dimB_payload") ptr0 dimB_payload3

deriving via Marshal.EquivStorable DimB instance RIP.Storable DimB

instance HasCField.HasCField DimB "dimB_tag" where

  type CFieldType DimB "dimB_tag" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "dimB_tag" (RIP.Ptr DimB) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"dimB_tag")

instance HasCField.HasCField DimB "dimB_payload" where

  type CFieldType DimB "dimB_payload" = DimPayloadB

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) DimPayloadB
         ) => RIP.HasField "dimB_payload" (RIP.Ptr DimB) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"dimB_payload")

{-| __C declaration:__ @struct \@AnonA_xy@

    __defined at:__ @types\/unions\/unions.h 35:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data AnonA_xy = AnonA_xy
  { anonA_xy_x :: RIP.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 35:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , anonA_xy_y :: RIP.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 35:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AnonA_xy where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw AnonA_xy where

  readRaw =
    \ptr0 ->
          pure AnonA_xy
      <*> HasCField.readRaw (RIP.Proxy @"anonA_xy_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"anonA_xy_y") ptr0

instance Marshal.WriteRaw AnonA_xy where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_xy anonA_xy_x2 anonA_xy_y3 ->
               HasCField.writeRaw (RIP.Proxy @"anonA_xy_x") ptr0 anonA_xy_x2
            >> HasCField.writeRaw (RIP.Proxy @"anonA_xy_y") ptr0 anonA_xy_y3

deriving via Marshal.EquivStorable AnonA_xy instance RIP.Storable AnonA_xy

instance HasCField.HasCField AnonA_xy "anonA_xy_x" where

  type CFieldType AnonA_xy "anonA_xy_x" = RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "anonA_xy_x" (RIP.Ptr AnonA_xy) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonA_xy_x")

instance HasCField.HasCField AnonA_xy "anonA_xy_y" where

  type CFieldType AnonA_xy "anonA_xy_y" = RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "anonA_xy_y" (RIP.Ptr AnonA_xy) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonA_xy_y")

{-| __C declaration:__ @struct \@AnonA_polar@

    __defined at:__ @types\/unions\/unions.h 36:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data AnonA_polar = AnonA_polar
  { anonA_polar_r :: RIP.CDouble
    {- ^ __C declaration:__ @r@

         __defined at:__ @types\/unions\/unions.h 36:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , anonA_polar_p :: RIP.CDouble
    {- ^ __C declaration:__ @p@

         __defined at:__ @types\/unions\/unions.h 36:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize AnonA_polar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw AnonA_polar where

  readRaw =
    \ptr0 ->
          pure AnonA_polar
      <*> HasCField.readRaw (RIP.Proxy @"anonA_polar_r") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"anonA_polar_p") ptr0

instance Marshal.WriteRaw AnonA_polar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_polar anonA_polar_r2 anonA_polar_p3 ->
               HasCField.writeRaw (RIP.Proxy @"anonA_polar_r") ptr0 anonA_polar_r2
            >> HasCField.writeRaw (RIP.Proxy @"anonA_polar_p") ptr0 anonA_polar_p3

deriving via Marshal.EquivStorable AnonA_polar instance RIP.Storable AnonA_polar

instance HasCField.HasCField AnonA_polar "anonA_polar_r" where

  type CFieldType AnonA_polar "anonA_polar_r" =
    RIP.CDouble

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "anonA_polar_r" (RIP.Ptr AnonA_polar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonA_polar_r")

instance HasCField.HasCField AnonA_polar "anonA_polar_p" where

  type CFieldType AnonA_polar "anonA_polar_p" =
    RIP.CDouble

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CDouble
         ) => RIP.HasField "anonA_polar_p" (RIP.Ptr AnonA_polar) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonA_polar_p")

{-| __C declaration:__ @union AnonA@

    __defined at:__ @types\/unions\/unions.h 34:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype AnonA = AnonA
  { unwrapAnonA :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 16) 8 instance Marshal.StaticSize AnonA

deriving via (RIP.SizedByteArray 16) 8 instance Marshal.ReadRaw AnonA

deriving via (RIP.SizedByteArray 16) 8 instance Marshal.WriteRaw AnonA

deriving via Marshal.EquivStorable AnonA instance RIP.Storable AnonA

{-|

  __See:__ 'set_anonA_xy'

__C declaration:__ @xy@

__defined at:__ @types\/unions\/unions.h 35:36@

__exported by:__ @types\/unions\/unions.h@
-}
get_anonA_xy ::
     AnonA
  -> AnonA_xy
get_anonA_xy = RIP.getUnionPayload

{-|

  __See:__ 'get_anonA_xy'

-}
set_anonA_xy ::
     AnonA_xy
  -> AnonA
set_anonA_xy = RIP.setUnionPayload

{-|

  __See:__ 'set_anonA_polar'

__C declaration:__ @polar@

__defined at:__ @types\/unions\/unions.h 36:36@

__exported by:__ @types\/unions\/unions.h@
-}
get_anonA_polar ::
     AnonA
  -> AnonA_polar
get_anonA_polar = RIP.getUnionPayload

{-|

  __See:__ 'get_anonA_polar'

-}
set_anonA_polar ::
     AnonA_polar
  -> AnonA
set_anonA_polar = RIP.setUnionPayload

instance HasCField.HasCField AnonA "anonA_xy" where

  type CFieldType AnonA "anonA_xy" = AnonA_xy

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) AnonA_xy
         ) => RIP.HasField "anonA_xy" (RIP.Ptr AnonA) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"anonA_xy")

instance HasCField.HasCField AnonA "anonA_polar" where

  type CFieldType AnonA "anonA_polar" = AnonA_polar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) AnonA_polar
         ) => RIP.HasField "anonA_polar" (RIP.Ptr AnonA) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"anonA_polar")
