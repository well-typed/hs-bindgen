{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.On_event_Aux(..)
    , Example.On_event(..)
    , Example.Buffer_Aux(..)
    , Example.Buffer
    , Example.Point(..)
    )
  where

import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure, type (~))
import {-# SOURCE #-} Example.FFI

{-| Auxiliary type used by 'On_event'

    __C declaration:__ @on_event@

    __defined at:__ @types\/boot\/ffi_module.h 6:15@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype On_event_Aux = On_event_Aux
  { unwrapOn_event_Aux :: BG.CInt -> IO BG.CInt
  }
  deriving stock (BG.Generic)

instance BG.ToFunPtr On_event_Aux where

  toFunPtr = hs_bindgen_70c2110bb36ed51a

instance BG.FromFunPtr On_event_Aux where

  fromFunPtr = hs_bindgen_a7705d093ccaa37a

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapOn_event_Aux" On_event_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> On_event_Aux {unwrapOn_event_Aux = y1}
      , BG.getField @"unwrapOn_event_Aux" x0
      )

instance ( ty ~ (BG.CInt -> IO BG.CInt)
         ) => BG.HasField "unwrapOn_event_Aux" (BG.Ptr On_event_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_event_Aux")

instance HasCField.HasCField On_event_Aux "unwrapOn_event_Aux" where

  type CFieldType On_event_Aux "unwrapOn_event_Aux" =
    BG.CInt -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @on_event@

    __defined at:__ @types\/boot\/ffi_module.h 6:15@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype On_event = On_event
  { unwrapOn_event :: BG.FunPtr On_event_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr On_event_Aux
         ) => BG.CompatHasField.HasField "unwrapOn_event" On_event ty where

  hasField =
    \x0 ->
      (\y1 ->
         On_event {unwrapOn_event = y1}, BG.getField @"unwrapOn_event" x0)

instance ( ty ~ BG.FunPtr On_event_Aux
         ) => BG.HasField "unwrapOn_event" (BG.Ptr On_event) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_event")

instance HasCField.HasCField On_event "unwrapOn_event" where

  type CFieldType On_event "unwrapOn_event" =
    BG.FunPtr On_event_Aux

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct buffer@

    __defined at:__ @types\/boot\/ffi_module.h 10:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
data Buffer_Aux = Buffer
  { buffer_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @types\/boot\/ffi_module.h 11:7@

         __exported by:__ @types\/boot\/ffi_module.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Buffer_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Buffer_Aux where

  readRaw =
    \ptr0 ->
          pure Buffer
      <*> HasCField.readRaw (BG.Proxy @"buffer_len") ptr0

instance Marshal.WriteRaw Buffer_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Buffer buffer_len2 ->
            HasCField.writeRaw (BG.Proxy @"buffer_len") ptr0 buffer_len2

deriving via Marshal.EquivStorable Buffer_Aux instance BG.Storable Buffer_Aux

deriving via Struct.IsStructViaReadRaw Buffer_Aux instance Struct.IsStruct Buffer_Aux

{-| __C declaration:__ @len@

    __defined at:__ @types\/boot\/ffi_module.h 11:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "buffer_len" Buffer_Aux ty where

  hasField =
    \x0 ->
      (\y1 ->
         Buffer {buffer_len = y1}, BG.getField @"buffer_len" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "buffer_len" (BG.Ptr Buffer_Aux) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"buffer_len")

instance HasCField.HasCField Buffer_Aux "buffer_len" where

  type CFieldType Buffer_Aux "buffer_len" = BG.CInt

  offset# = \_ -> \_ -> 0

instance FLAM.Offset BG.CChar Buffer_Aux where

  offset = \_proxy0 -> 4

{-| __C declaration:__ @struct buffer@

    __defined at:__ @types\/boot\/ffi_module.h 10:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux

{-| __C declaration:__ @struct point@

    __defined at:__ @types\/boot\/ffi_module.h 17:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
data Point = Point
  { point_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/boot\/ffi_module.h 18:7@

         __exported by:__ @types\/boot\/ffi_module.h@
    -}
  , point_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/boot\/ffi_module.h 19:7@

         __exported by:__ @types\/boot\/ffi_module.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HasCField.readRaw (BG.Proxy @"point_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point_y") ptr0

instance Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 ->
               HasCField.writeRaw (BG.Proxy @"point_x") ptr0 point_x2
            >> HasCField.writeRaw (BG.Proxy @"point_y") ptr0 point_y3

deriving via Marshal.EquivStorable Point instance BG.Storable Point

deriving via Struct.IsStructViaReadRaw Point instance Struct.IsStruct Point

{-| __C declaration:__ @x@

    __defined at:__ @types\/boot\/ffi_module.h 18:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_x" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point {point_x = y1, point_y = BG.getField @"point_y" x0}
      , BG.getField @"point_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_x" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_x")

instance HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @types\/boot\/ffi_module.h 19:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_y" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point {point_y = y1, point_x = BG.getField @"point_x" x0}
      , BG.getField @"point_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_y" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_y")

instance HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = BG.CInt

  offset# = \_ -> \_ -> 4

instance BG.ToFunPtr (BG.Ptr Point -> IO BG.CInt) where

  toFunPtr = hs_bindgen_ce6081d63db40b52

instance BG.FromFunPtr (BG.Ptr Point -> IO BG.CInt) where

  fromFunPtr = hs_bindgen_9ee31aa034cd5991
