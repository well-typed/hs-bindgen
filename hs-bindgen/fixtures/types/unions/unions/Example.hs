{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Array.Byte
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Eq, Int, Show, pure)

{-| __C declaration:__ @struct Dim2@

    __defined at:__ @types\/unions\/unions.h 1:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim2 = Dim2
  { dim2_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 2:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim2_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 3:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Dim2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Dim2 where

  readRaw =
    \ptr0 ->
          pure Dim2
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim2_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim2_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Dim2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim2 dim2_x2 dim2_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim2_x") ptr0 dim2_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim2_y") ptr0 dim2_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Dim2 instance F.Storable Dim2

instance HsBindgen.Runtime.HasCField.HasCField Dim2 "dim2_x" where

  type CFieldType Dim2 "dim2_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim2) "dim2_x")
         ) => GHC.Records.HasField "dim2_x" (Ptr.Ptr Dim2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim2_x")

instance HsBindgen.Runtime.HasCField.HasCField Dim2 "dim2_y" where

  type CFieldType Dim2 "dim2_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim2) "dim2_y")
         ) => GHC.Records.HasField "dim2_y" (Ptr.Ptr Dim2) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim2_y")

{-| __C declaration:__ @struct Dim3@

    __defined at:__ @types\/unions\/unions.h 6:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim3 = Dim3
  { dim3_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 7:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 8:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_z :: FC.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/unions\/unions.h 9:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Dim3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Dim3 where

  readRaw =
    \ptr0 ->
          pure Dim3
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim3_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim3_y") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim3_z") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Dim3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim3 dim3_x2 dim3_y3 dim3_z4 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim3_x") ptr0 dim3_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim3_y") ptr0 dim3_y3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim3_z") ptr0 dim3_z4

deriving via HsBindgen.Runtime.Marshal.EquivStorable Dim3 instance F.Storable Dim3

instance HsBindgen.Runtime.HasCField.HasCField Dim3 "dim3_x" where

  type CFieldType Dim3 "dim3_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim3) "dim3_x")
         ) => GHC.Records.HasField "dim3_x" (Ptr.Ptr Dim3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim3_x")

instance HsBindgen.Runtime.HasCField.HasCField Dim3 "dim3_y" where

  type CFieldType Dim3 "dim3_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim3) "dim3_y")
         ) => GHC.Records.HasField "dim3_y" (Ptr.Ptr Dim3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim3_y")

instance HsBindgen.Runtime.HasCField.HasCField Dim3 "dim3_z" where

  type CFieldType Dim3 "dim3_z" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim3) "dim3_z")
         ) => GHC.Records.HasField "dim3_z" (Ptr.Ptr Dim3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim3_z")

{-| __C declaration:__ @union DimPayload@

    __defined at:__ @types\/unions\/unions.h 12:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayload = DimPayload
  { unwrapDimPayload :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.StaticSize DimPayload

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.ReadRaw DimPayload

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.WriteRaw DimPayload

deriving via HsBindgen.Runtime.Marshal.EquivStorable DimPayload instance F.Storable DimPayload

{-|

  __See:__ 'set_dimPayload_dim2'

__C declaration:__ @dim2@

__defined at:__ @types\/unions\/unions.h 13:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayload_dim2 ::
     DimPayload
  -> Dim2
get_dimPayload_dim2 =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim2'

-}
set_dimPayload_dim2 ::
     Dim2
  -> DimPayload
set_dimPayload_dim2 =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_dimPayload_dim3'

__C declaration:__ @dim3@

__defined at:__ @types\/unions\/unions.h 14:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayload_dim3 ::
     DimPayload
  -> Dim2
get_dimPayload_dim3 =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayload_dim3'

-}
set_dimPayload_dim3 ::
     Dim2
  -> DimPayload
set_dimPayload_dim3 =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField DimPayload "dimPayload_dim2" where

  type CFieldType DimPayload "dimPayload_dim2" = Dim2

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimPayload) "dimPayload_dim2")
         ) => GHC.Records.HasField "dimPayload_dim2" (Ptr.Ptr DimPayload) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimPayload_dim2")

instance HsBindgen.Runtime.HasCField.HasCField DimPayload "dimPayload_dim3" where

  type CFieldType DimPayload "dimPayload_dim3" = Dim2

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimPayload) "dimPayload_dim3")
         ) => GHC.Records.HasField "dimPayload_dim3" (Ptr.Ptr DimPayload) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimPayload_dim3")

{-| __C declaration:__ @struct Dim@

    __defined at:__ @types\/unions\/unions.h 17:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim = Dim
  { dim_tag :: FC.CInt
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
  deriving stock (GHC.Generics.Generic)

instance HsBindgen.Runtime.Marshal.StaticSize Dim where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Dim where

  readRaw =
    \ptr0 ->
          pure Dim
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim_tag") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dim_payload") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Dim where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim dim_tag2 dim_payload3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim_tag") ptr0 dim_tag2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dim_payload") ptr0 dim_payload3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Dim instance F.Storable Dim

instance HsBindgen.Runtime.HasCField.HasCField Dim "dim_tag" where

  type CFieldType Dim "dim_tag" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim) "dim_tag")
         ) => GHC.Records.HasField "dim_tag" (Ptr.Ptr Dim) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim_tag")

instance HsBindgen.Runtime.HasCField.HasCField Dim "dim_payload" where

  type CFieldType Dim "dim_payload" = DimPayload

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Dim) "dim_payload")
         ) => GHC.Records.HasField "dim_payload" (Ptr.Ptr Dim) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dim_payload")

{-| __C declaration:__ @union DimPayloadB@

    __defined at:__ @types\/unions\/unions.h 23:15@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayloadB = DimPayloadB
  { unwrapDimPayloadB :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.StaticSize DimPayloadB

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.ReadRaw DimPayloadB

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.WriteRaw DimPayloadB

deriving via HsBindgen.Runtime.Marshal.EquivStorable DimPayloadB instance F.Storable DimPayloadB

{-|

  __See:__ 'set_dimPayloadB_dim2'

__C declaration:__ @dim2@

__defined at:__ @types\/unions\/unions.h 24:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayloadB_dim2 ::
     DimPayloadB
  -> Dim2
get_dimPayloadB_dim2 =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim2'

-}
set_dimPayloadB_dim2 ::
     Dim2
  -> DimPayloadB
set_dimPayloadB_dim2 =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_dimPayloadB_dim3'

__C declaration:__ @dim3@

__defined at:__ @types\/unions\/unions.h 25:17@

__exported by:__ @types\/unions\/unions.h@
-}
get_dimPayloadB_dim3 ::
     DimPayloadB
  -> Dim2
get_dimPayloadB_dim3 =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_dimPayloadB_dim3'

-}
set_dimPayloadB_dim3 ::
     Dim2
  -> DimPayloadB
set_dimPayloadB_dim3 =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField DimPayloadB "dimPayloadB_dim2" where

  type CFieldType DimPayloadB "dimPayloadB_dim2" = Dim2

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimPayloadB) "dimPayloadB_dim2")
         ) => GHC.Records.HasField "dimPayloadB_dim2" (Ptr.Ptr DimPayloadB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimPayloadB_dim2")

instance HsBindgen.Runtime.HasCField.HasCField DimPayloadB "dimPayloadB_dim3" where

  type CFieldType DimPayloadB "dimPayloadB_dim3" = Dim2

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimPayloadB) "dimPayloadB_dim3")
         ) => GHC.Records.HasField "dimPayloadB_dim3" (Ptr.Ptr DimPayloadB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimPayloadB_dim3")

{-| __C declaration:__ @struct DimB@

    __defined at:__ @types\/unions\/unions.h 28:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data DimB = DimB
  { dimB_tag :: FC.CInt
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
  deriving stock (GHC.Generics.Generic)

instance HsBindgen.Runtime.Marshal.StaticSize DimB where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw DimB where

  readRaw =
    \ptr0 ->
          pure DimB
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dimB_tag") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"dimB_payload") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw DimB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DimB dimB_tag2 dimB_payload3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dimB_tag") ptr0 dimB_tag2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"dimB_payload") ptr0 dimB_payload3

deriving via HsBindgen.Runtime.Marshal.EquivStorable DimB instance F.Storable DimB

instance HsBindgen.Runtime.HasCField.HasCField DimB "dimB_tag" where

  type CFieldType DimB "dimB_tag" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimB) "dimB_tag")
         ) => GHC.Records.HasField "dimB_tag" (Ptr.Ptr DimB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimB_tag")

instance HsBindgen.Runtime.HasCField.HasCField DimB "dimB_payload" where

  type CFieldType DimB "dimB_payload" = DimPayloadB

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType DimB) "dimB_payload")
         ) => GHC.Records.HasField "dimB_payload" (Ptr.Ptr DimB) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"dimB_payload")

{-| __C declaration:__ @struct \@AnonA_xy@

    __defined at:__ @types\/unions\/unions.h 35:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data AnonA_xy = AnonA_xy
  { anonA_xy_x :: FC.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 35:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , anonA_xy_y :: FC.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 35:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize AnonA_xy where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw AnonA_xy where

  readRaw =
    \ptr0 ->
          pure AnonA_xy
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"anonA_xy_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"anonA_xy_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw AnonA_xy where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_xy anonA_xy_x2 anonA_xy_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"anonA_xy_x") ptr0 anonA_xy_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"anonA_xy_y") ptr0 anonA_xy_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable AnonA_xy instance F.Storable AnonA_xy

instance HsBindgen.Runtime.HasCField.HasCField AnonA_xy "anonA_xy_x" where

  type CFieldType AnonA_xy "anonA_xy_x" = FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA_xy) "anonA_xy_x")
         ) => GHC.Records.HasField "anonA_xy_x" (Ptr.Ptr AnonA_xy) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_xy_x")

instance HsBindgen.Runtime.HasCField.HasCField AnonA_xy "anonA_xy_y" where

  type CFieldType AnonA_xy "anonA_xy_y" = FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA_xy) "anonA_xy_y")
         ) => GHC.Records.HasField "anonA_xy_y" (Ptr.Ptr AnonA_xy) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_xy_y")

{-| __C declaration:__ @struct \@AnonA_polar@

    __defined at:__ @types\/unions\/unions.h 36:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data AnonA_polar = AnonA_polar
  { anonA_polar_r :: FC.CDouble
    {- ^ __C declaration:__ @r@

         __defined at:__ @types\/unions\/unions.h 36:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , anonA_polar_p :: FC.CDouble
    {- ^ __C declaration:__ @p@

         __defined at:__ @types\/unions\/unions.h 36:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize AnonA_polar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw AnonA_polar where

  readRaw =
    \ptr0 ->
          pure AnonA_polar
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"anonA_polar_r") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"anonA_polar_p") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw AnonA_polar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          AnonA_polar anonA_polar_r2 anonA_polar_p3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"anonA_polar_r") ptr0 anonA_polar_r2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"anonA_polar_p") ptr0 anonA_polar_p3

deriving via HsBindgen.Runtime.Marshal.EquivStorable AnonA_polar instance F.Storable AnonA_polar

instance HsBindgen.Runtime.HasCField.HasCField AnonA_polar "anonA_polar_r" where

  type CFieldType AnonA_polar "anonA_polar_r" =
    FC.CDouble

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA_polar) "anonA_polar_r")
         ) => GHC.Records.HasField "anonA_polar_r" (Ptr.Ptr AnonA_polar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_polar_r")

instance HsBindgen.Runtime.HasCField.HasCField AnonA_polar "anonA_polar_p" where

  type CFieldType AnonA_polar "anonA_polar_p" =
    FC.CDouble

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA_polar) "anonA_polar_p")
         ) => GHC.Records.HasField "anonA_polar_p" (Ptr.Ptr AnonA_polar) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_polar_p")

{-| __C declaration:__ @union AnonA@

    __defined at:__ @types\/unions\/unions.h 34:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype AnonA = AnonA
  { unwrapAnonA :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 8 instance HsBindgen.Runtime.Marshal.StaticSize AnonA

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 8 instance HsBindgen.Runtime.Marshal.ReadRaw AnonA

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 8 instance HsBindgen.Runtime.Marshal.WriteRaw AnonA

deriving via HsBindgen.Runtime.Marshal.EquivStorable AnonA instance F.Storable AnonA

{-|

  __See:__ 'set_anonA_xy'

__C declaration:__ @xy@

__defined at:__ @types\/unions\/unions.h 35:36@

__exported by:__ @types\/unions\/unions.h@
-}
get_anonA_xy ::
     AnonA
  -> AnonA_xy
get_anonA_xy =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_anonA_xy'

-}
set_anonA_xy ::
     AnonA_xy
  -> AnonA
set_anonA_xy =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_anonA_polar'

__C declaration:__ @polar@

__defined at:__ @types\/unions\/unions.h 36:36@

__exported by:__ @types\/unions\/unions.h@
-}
get_anonA_polar ::
     AnonA
  -> AnonA_polar
get_anonA_polar =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_anonA_polar'

-}
set_anonA_polar ::
     AnonA_polar
  -> AnonA
set_anonA_polar =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField AnonA "anonA_xy" where

  type CFieldType AnonA "anonA_xy" = AnonA_xy

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA) "anonA_xy")
         ) => GHC.Records.HasField "anonA_xy" (Ptr.Ptr AnonA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_xy")

instance HsBindgen.Runtime.HasCField.HasCField AnonA "anonA_polar" where

  type CFieldType AnonA "anonA_polar" = AnonA_polar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType AnonA) "anonA_polar")
         ) => GHC.Records.HasField "anonA_polar" (Ptr.Ptr AnonA) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"anonA_polar")
