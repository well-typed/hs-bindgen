{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.Dim2(..)
    , Example.Dim3(..)
    , Example.DimPayload(..)
    , Example.Dim(..)
    , Example.DimPayloadB(..)
    , Example.DimB(..)
    , Example.UntaggedA_xy(..)
    , Example.UntaggedA_polar(..)
    , Example.UntaggedA(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union

{-| __C declaration:__ @struct Dim2@

    __defined at:__ @types\/unions\/unions.h 1:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim2 = Dim2
  { dim2_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 2:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim2_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 3:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Dim2 where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim2 where

  readRaw =
    \ptr0 ->
          pure Dim2
      <*> HasCField.readRaw (BG.Proxy @"dim2_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dim2_y") ptr0

instance Marshal.WriteRaw Dim2 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim2 dim2_x2 dim2_y3 ->
               HasCField.writeRaw (BG.Proxy @"dim2_x") ptr0 dim2_x2
            >> HasCField.writeRaw (BG.Proxy @"dim2_y") ptr0 dim2_y3

deriving via Marshal.EquivStorable Dim2 instance BG.Storable Dim2

deriving via Struct.IsStructViaReadRaw Dim2 instance Struct.IsStruct Dim2

{-| __C declaration:__ @x@

    __defined at:__ @types\/unions\/unions.h 2:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim2_x" Dim2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim2 {dim2_x = y1, dim2_y = BG.getField @"dim2_y" x0}
      , BG.getField @"dim2_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim2_x" (BG.Ptr Dim2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim2_x")

instance HasCField.HasCField Dim2 "dim2_x" where

  type CFieldType Dim2 "dim2_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/unions\/unions.h 3:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim2_y" Dim2 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim2 {dim2_y = y1, dim2_x = BG.getField @"dim2_x" x0}
      , BG.getField @"dim2_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim2_y" (BG.Ptr Dim2) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim2_y")

instance HasCField.HasCField Dim2 "dim2_y" where

  type CFieldType Dim2 "dim2_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct Dim3@

    __defined at:__ @types\/unions\/unions.h 6:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim3 = Dim3
  { dim3_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 7:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 8:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , dim3_z :: BG.CInt
    {- ^ __C declaration:__ @z@

         __defined at:__ @types\/unions\/unions.h 9:9@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Dim3 where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim3 where

  readRaw =
    \ptr0 ->
          pure Dim3
      <*> HasCField.readRaw (BG.Proxy @"dim3_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dim3_y") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dim3_z") ptr0

instance Marshal.WriteRaw Dim3 where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim3 dim3_x2 dim3_y3 dim3_z4 ->
               HasCField.writeRaw (BG.Proxy @"dim3_x") ptr0 dim3_x2
            >> HasCField.writeRaw (BG.Proxy @"dim3_y") ptr0 dim3_y3
            >> HasCField.writeRaw (BG.Proxy @"dim3_z") ptr0 dim3_z4

deriving via Marshal.EquivStorable Dim3 instance BG.Storable Dim3

deriving via Struct.IsStructViaReadRaw Dim3 instance Struct.IsStruct Dim3

{-| __C declaration:__ @x@

    __defined at:__ @types\/unions\/unions.h 7:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim3_x" Dim3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim3 { dim3_x = y1
               , dim3_y = BG.getField @"dim3_y" x0
               , dim3_z = BG.getField @"dim3_z" x0
               }
      , BG.getField @"dim3_x" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim3_x" (BG.Ptr Dim3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim3_x")

instance HasCField.HasCField Dim3 "dim3_x" where

  type CFieldType Dim3 "dim3_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/unions\/unions.h 8:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim3_y" Dim3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim3 { dim3_y = y1
               , dim3_x = BG.getField @"dim3_x" x0
               , dim3_z = BG.getField @"dim3_z" x0
               }
      , BG.getField @"dim3_y" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim3_y" (BG.Ptr Dim3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim3_y")

instance HasCField.HasCField Dim3 "dim3_y" where

  type CFieldType Dim3 "dim3_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @z@

    __defined at:__ @types\/unions\/unions.h 9:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim3_z" Dim3 ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim3 { dim3_z = y1
               , dim3_x = BG.getField @"dim3_x" x0
               , dim3_y = BG.getField @"dim3_y" x0
               }
      , BG.getField @"dim3_z" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim3_z" (BG.Ptr Dim3) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim3_z")

instance HasCField.HasCField Dim3 "dim3_z" where

  type CFieldType Dim3 "dim3_z" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @union DimPayload@

    __defined at:__ @types\/unions\/unions.h 12:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayload = DimPayload
  { unwrapDimPayload :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 4 instance Marshal.StaticSize DimPayload

deriving via BG.SizedByteArray 8 4 instance Marshal.ReadRaw DimPayload

deriving via BG.SizedByteArray 8 4 instance Marshal.WriteRaw DimPayload

deriving via Marshal.EquivStorable DimPayload instance BG.Storable DimPayload

deriving via BG.SizedByteArray 8 4 instance Union.IsUnion DimPayload

{-| __C declaration:__ @dim2@

    __defined at:__ @types\/unions\/unions.h 13:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ Dim2) => BG.HasField "dimPayload_dim2" DimPayload ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @dim2@

    __defined at:__ @types\/unions\/unions.h 13:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ Dim2
         ) => BG.CompatHasField.HasField "dimPayload_dim2" DimPayload ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"dimPayload_dim2" x0)

instance ( ty ~ Dim2
         ) => BG.HasField "dimPayload_dim2" (BG.Ptr DimPayload) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dimPayload_dim2")

instance HasCField.HasCField DimPayload "dimPayload_dim2" where

  type CFieldType DimPayload "dimPayload_dim2" = Dim2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @dim3@

    __defined at:__ @types\/unions\/unions.h 14:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ Dim2) => BG.HasField "dimPayload_dim3" DimPayload ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @dim3@

    __defined at:__ @types\/unions\/unions.h 14:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ Dim2
         ) => BG.CompatHasField.HasField "dimPayload_dim3" DimPayload ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"dimPayload_dim3" x0)

instance ( ty ~ Dim2
         ) => BG.HasField "dimPayload_dim3" (BG.Ptr DimPayload) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dimPayload_dim3")

instance HasCField.HasCField DimPayload "dimPayload_dim3" where

  type CFieldType DimPayload "dimPayload_dim3" = Dim2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct Dim@

    __defined at:__ @types\/unions\/unions.h 17:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data Dim = Dim
  { dim_tag :: BG.CInt
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
  deriving stock (BG.Generic)

instance Marshal.StaticSize Dim where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Dim where

  readRaw =
    \ptr0 ->
          pure Dim
      <*> HasCField.readRaw (BG.Proxy @"dim_tag") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dim_payload") ptr0

instance Marshal.WriteRaw Dim where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Dim dim_tag2 dim_payload3 ->
               HasCField.writeRaw (BG.Proxy @"dim_tag") ptr0 dim_tag2
            >> HasCField.writeRaw (BG.Proxy @"dim_payload") ptr0 dim_payload3

deriving via Marshal.EquivStorable Dim instance BG.Storable Dim

deriving via Struct.IsStructViaReadRaw Dim instance Struct.IsStruct Dim

{-| __C declaration:__ @tag@

    __defined at:__ @types\/unions\/unions.h 18:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dim_tag" Dim ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim {dim_tag = y1, dim_payload = BG.getField @"dim_payload" x0}
      , BG.getField @"dim_tag" x0
      )

instance (ty ~ BG.CInt) => BG.HasField "dim_tag" (BG.Ptr Dim) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dim_tag")

instance HasCField.HasCField Dim "dim_tag" where

  type CFieldType Dim "dim_tag" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @payload@

    __defined at:__ @types\/unions\/unions.h 19:22@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ DimPayload
         ) => BG.CompatHasField.HasField "dim_payload" Dim ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Dim {dim_payload = y1, dim_tag = BG.getField @"dim_tag" x0}
      , BG.getField @"dim_payload" x0
      )

instance ( ty ~ DimPayload
         ) => BG.HasField "dim_payload" (BG.Ptr Dim) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dim_payload")

instance HasCField.HasCField Dim "dim_payload" where

  type CFieldType Dim "dim_payload" = DimPayload

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @union DimPayloadB@

    __defined at:__ @types\/unions\/unions.h 23:15@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype DimPayloadB = DimPayloadB
  { unwrapDimPayloadB :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 8 4 instance Marshal.StaticSize DimPayloadB

deriving via BG.SizedByteArray 8 4 instance Marshal.ReadRaw DimPayloadB

deriving via BG.SizedByteArray 8 4 instance Marshal.WriteRaw DimPayloadB

deriving via Marshal.EquivStorable DimPayloadB instance BG.Storable DimPayloadB

deriving via BG.SizedByteArray 8 4 instance Union.IsUnion DimPayloadB

{-| __C declaration:__ @dim2@

    __defined at:__ @types\/unions\/unions.h 24:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ Dim2) => BG.HasField "dimPayloadB_dim2" DimPayloadB ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @dim2@

    __defined at:__ @types\/unions\/unions.h 24:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ Dim2
         ) => BG.CompatHasField.HasField "dimPayloadB_dim2" DimPayloadB ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"dimPayloadB_dim2" x0)

instance ( ty ~ Dim2
         ) => BG.HasField "dimPayloadB_dim2" (BG.Ptr DimPayloadB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dimPayloadB_dim2")

instance HasCField.HasCField DimPayloadB "dimPayloadB_dim2" where

  type CFieldType DimPayloadB "dimPayloadB_dim2" = Dim2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @dim3@

    __defined at:__ @types\/unions\/unions.h 25:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ Dim2) => BG.HasField "dimPayloadB_dim3" DimPayloadB ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @dim3@

    __defined at:__ @types\/unions\/unions.h 25:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ Dim2
         ) => BG.CompatHasField.HasField "dimPayloadB_dim3" DimPayloadB ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"dimPayloadB_dim3" x0)

instance ( ty ~ Dim2
         ) => BG.HasField "dimPayloadB_dim3" (BG.Ptr DimPayloadB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dimPayloadB_dim3")

instance HasCField.HasCField DimPayloadB "dimPayloadB_dim3" where

  type CFieldType DimPayloadB "dimPayloadB_dim3" = Dim2

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct DimB@

    __defined at:__ @types\/unions\/unions.h 28:8@

    __exported by:__ @types\/unions\/unions.h@
-}
data DimB = DimB
  { dimB_tag :: BG.CInt
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
  deriving stock (BG.Generic)

instance Marshal.StaticSize DimB where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw DimB where

  readRaw =
    \ptr0 ->
          pure DimB
      <*> HasCField.readRaw (BG.Proxy @"dimB_tag") ptr0
      <*> HasCField.readRaw (BG.Proxy @"dimB_payload") ptr0

instance Marshal.WriteRaw DimB where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          DimB dimB_tag2 dimB_payload3 ->
               HasCField.writeRaw (BG.Proxy @"dimB_tag") ptr0 dimB_tag2
            >> HasCField.writeRaw (BG.Proxy @"dimB_payload") ptr0 dimB_payload3

deriving via Marshal.EquivStorable DimB instance BG.Storable DimB

deriving via Struct.IsStructViaReadRaw DimB instance Struct.IsStruct DimB

{-| __C declaration:__ @tag@

    __defined at:__ @types\/unions\/unions.h 29:9@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "dimB_tag" DimB ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DimB {dimB_tag = y1, dimB_payload = BG.getField @"dimB_payload" x0}
      , BG.getField @"dimB_tag" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "dimB_tag" (BG.Ptr DimB) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"dimB_tag")

instance HasCField.HasCField DimB "dimB_tag" where

  type CFieldType DimB "dimB_tag" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @payload@

    __defined at:__ @types\/unions\/unions.h 30:17@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ DimPayloadB
         ) => BG.CompatHasField.HasField "dimB_payload" DimB ty where

  hasField =
    \x0 ->
      ( \y1 ->
          DimB {dimB_payload = y1, dimB_tag = BG.getField @"dimB_tag" x0}
      , BG.getField @"dimB_payload" x0
      )

instance ( ty ~ DimPayloadB
         ) => BG.HasField "dimB_payload" (BG.Ptr DimB) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"dimB_payload")

instance HasCField.HasCField DimB "dimB_payload" where

  type CFieldType DimB "dimB_payload" = DimPayloadB

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @struct \@UntaggedA_xy@

    __defined at:__ @types\/unions\/unions.h 35:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data UntaggedA_xy = UntaggedA_xy
  { untaggedA_xy_x :: BG.CDouble
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/unions\/unions.h 35:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , untaggedA_xy_y :: BG.CDouble
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/unions\/unions.h 35:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UntaggedA_xy where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw UntaggedA_xy where

  readRaw =
    \ptr0 ->
          pure UntaggedA_xy
      <*> HasCField.readRaw (BG.Proxy @"untaggedA_xy_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"untaggedA_xy_y") ptr0

instance Marshal.WriteRaw UntaggedA_xy where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedA_xy untaggedA_xy_x2 untaggedA_xy_y3 ->
               HasCField.writeRaw (BG.Proxy @"untaggedA_xy_x") ptr0 untaggedA_xy_x2
            >> HasCField.writeRaw (BG.Proxy @"untaggedA_xy_y") ptr0 untaggedA_xy_y3

deriving via Marshal.EquivStorable UntaggedA_xy instance BG.Storable UntaggedA_xy

deriving via Struct.IsStructViaReadRaw UntaggedA_xy instance Struct.IsStruct UntaggedA_xy

{-| __C declaration:__ @x@

    __defined at:__ @types\/unions\/unions.h 35:21@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "untaggedA_xy_x" UntaggedA_xy ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedA_xy {untaggedA_xy_x = y1, untaggedA_xy_y = BG.getField @"untaggedA_xy_y" x0}
      , BG.getField @"untaggedA_xy_x" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "untaggedA_xy_x" (BG.Ptr UntaggedA_xy) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_xy_x")

instance HasCField.HasCField UntaggedA_xy "untaggedA_xy_x" where

  type CFieldType UntaggedA_xy "untaggedA_xy_x" =
    BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/unions\/unions.h 35:31@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "untaggedA_xy_y" UntaggedA_xy ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedA_xy {untaggedA_xy_y = y1, untaggedA_xy_x = BG.getField @"untaggedA_xy_x" x0}
      , BG.getField @"untaggedA_xy_y" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "untaggedA_xy_y" (BG.Ptr UntaggedA_xy) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_xy_y")

instance HasCField.HasCField UntaggedA_xy "untaggedA_xy_y" where

  type CFieldType UntaggedA_xy "untaggedA_xy_y" =
    BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @struct \@UntaggedA_polar@

    __defined at:__ @types\/unions\/unions.h 36:5@

    __exported by:__ @types\/unions\/unions.h@
-}
data UntaggedA_polar = UntaggedA_polar
  { untaggedA_polar_r :: BG.CDouble
    {- ^ __C declaration:__ @r@

         __defined at:__ @types\/unions\/unions.h 36:21@

         __exported by:__ @types\/unions\/unions.h@
    -}
  , untaggedA_polar_p :: BG.CDouble
    {- ^ __C declaration:__ @p@

         __defined at:__ @types\/unions\/unions.h 36:31@

         __exported by:__ @types\/unions\/unions.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize UntaggedA_polar where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw UntaggedA_polar where

  readRaw =
    \ptr0 ->
          pure UntaggedA_polar
      <*> HasCField.readRaw (BG.Proxy @"untaggedA_polar_r") ptr0
      <*> HasCField.readRaw (BG.Proxy @"untaggedA_polar_p") ptr0

instance Marshal.WriteRaw UntaggedA_polar where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          UntaggedA_polar untaggedA_polar_r2 untaggedA_polar_p3 ->
               HasCField.writeRaw (BG.Proxy @"untaggedA_polar_r") ptr0 untaggedA_polar_r2
            >> HasCField.writeRaw (BG.Proxy @"untaggedA_polar_p") ptr0 untaggedA_polar_p3

deriving via Marshal.EquivStorable UntaggedA_polar instance BG.Storable UntaggedA_polar

deriving via Struct.IsStructViaReadRaw UntaggedA_polar instance Struct.IsStruct UntaggedA_polar

{-| __C declaration:__ @r@

    __defined at:__ @types\/unions\/unions.h 36:21@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "untaggedA_polar_r" UntaggedA_polar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedA_polar { untaggedA_polar_r = y1
                          , untaggedA_polar_p = BG.getField @"untaggedA_polar_p" x0
                          }
      , BG.getField @"untaggedA_polar_r" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "untaggedA_polar_r" (BG.Ptr UntaggedA_polar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_polar_r")

instance HasCField.HasCField UntaggedA_polar "untaggedA_polar_r" where

  type CFieldType UntaggedA_polar "untaggedA_polar_r" =
    BG.CDouble

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @p@

    __defined at:__ @types\/unions\/unions.h 36:31@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ BG.CDouble
         ) => BG.CompatHasField.HasField "untaggedA_polar_p" UntaggedA_polar ty where

  hasField =
    \x0 ->
      ( \y1 ->
          UntaggedA_polar { untaggedA_polar_p = y1
                          , untaggedA_polar_r = BG.getField @"untaggedA_polar_r" x0
                          }
      , BG.getField @"untaggedA_polar_p" x0
      )

instance ( ty ~ BG.CDouble
         ) => BG.HasField "untaggedA_polar_p" (BG.Ptr UntaggedA_polar) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_polar_p")

instance HasCField.HasCField UntaggedA_polar "untaggedA_polar_p" where

  type CFieldType UntaggedA_polar "untaggedA_polar_p" =
    BG.CDouble

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @union UntaggedA@

    __defined at:__ @types\/unions\/unions.h 34:7@

    __exported by:__ @types\/unions\/unions.h@
-}
newtype UntaggedA = UntaggedA
  { unwrapUntaggedA :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 16 8 instance Marshal.StaticSize UntaggedA

deriving via BG.SizedByteArray 16 8 instance Marshal.ReadRaw UntaggedA

deriving via BG.SizedByteArray 16 8 instance Marshal.WriteRaw UntaggedA

deriving via Marshal.EquivStorable UntaggedA instance BG.Storable UntaggedA

deriving via BG.SizedByteArray 16 8 instance Union.IsUnion UntaggedA

{-| __C declaration:__ @xy@

    __defined at:__ @types\/unions\/unions.h 35:36@

    __exported by:__ @types\/unions\/unions.h@
-}
instance (ty ~ UntaggedA_xy) => BG.HasField "untaggedA_xy" UntaggedA ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @xy@

    __defined at:__ @types\/unions\/unions.h 35:36@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ UntaggedA_xy
         ) => BG.CompatHasField.HasField "untaggedA_xy" UntaggedA ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"untaggedA_xy" x0)

instance ( ty ~ UntaggedA_xy
         ) => BG.HasField "untaggedA_xy" (BG.Ptr UntaggedA) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_xy")

instance HasCField.HasCField UntaggedA "untaggedA_xy" where

  type CFieldType UntaggedA "untaggedA_xy" =
    UntaggedA_xy

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @polar@

    __defined at:__ @types\/unions\/unions.h 36:36@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ UntaggedA_polar
         ) => BG.HasField "untaggedA_polar" UntaggedA ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @polar@

    __defined at:__ @types\/unions\/unions.h 36:36@

    __exported by:__ @types\/unions\/unions.h@
-}
instance ( ty ~ UntaggedA_polar
         ) => BG.CompatHasField.HasField "untaggedA_polar" UntaggedA ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"untaggedA_polar" x0)

instance ( ty ~ UntaggedA_polar
         ) => BG.HasField "untaggedA_polar" (BG.Ptr UntaggedA) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"untaggedA_polar")

instance HasCField.HasCField UntaggedA "untaggedA_polar" where

  type CFieldType UntaggedA "untaggedA_polar" =
    UntaggedA_polar

  offset# = \_ -> \_ -> 0
