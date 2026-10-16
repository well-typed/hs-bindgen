{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example
    ( Example.On_event_Aux(..)
    , Example.On_event(..)
    , Example.Buffer_Aux(..)
    , Example.Buffer
    , Example.Point(..)
    , Example.Colour(..)
    , pattern Example.RED
    , pattern Example.GREEN
    , pattern Example.BLUE
    , Example.Value(..)
    , Example.Status(..)
    , Example.Sample(..)
    , Example.COUNT_T(..)
    , Example.mAX_EVENTS
    , Example.Handle
    )
  where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.LibC
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import qualified HsBindgen.Runtime.Union as Union
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))
import {-# SOURCE #-} Example.FFI

{-| Auxiliary type used by 'On_event'

    __C declaration:__ @on_event@

    __defined at:__ @types\/boot\/ffi_module.h 13:15@

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

    __defined at:__ @types\/boot\/ffi_module.h 13:15@

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

    __defined at:__ @types\/boot\/ffi_module.h 17:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
data Buffer_Aux = Buffer
  { buffer_len :: BG.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @types\/boot\/ffi_module.h 18:7@

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

    __defined at:__ @types\/boot\/ffi_module.h 18:7@

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

    __defined at:__ @types\/boot\/ffi_module.h 17:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
type Buffer = FLAM.WithFlam BG.CChar Buffer_Aux

{-| __C declaration:__ @struct point@

    __defined at:__ @types\/boot\/ffi_module.h 24:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
data Point = Point
  { point_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @types\/boot\/ffi_module.h 25:7@

         __exported by:__ @types\/boot\/ffi_module.h@
    -}
  , point_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @types\/boot\/ffi_module.h 26:7@

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

    __defined at:__ @types\/boot\/ffi_module.h 25:7@

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

    __defined at:__ @types\/boot\/ffi_module.h 26:7@

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

{-| __C declaration:__ @enum colour@

    __defined at:__ @types\/boot\/ffi_module.h 37:6@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype Colour = Colour
  { unwrapColour :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord)
  deriving newtype (BG.HasFFIType)

instance Marshal.StaticSize Colour where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Colour where

  readRaw =
    \ptr0 ->
          pure Colour
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw Colour where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Colour unwrapColour2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrapColour2

deriving via Marshal.EquivStorable Colour instance BG.Storable Colour

deriving via BG.CUInt instance BG.Prim Colour

instance CEnum.CEnum Colour where

  type CEnumZ Colour = BG.CUInt

  toCEnum = Colour

  fromCEnum = BG.getField @"unwrapColour"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, BG.singleton "RED"), (1, BG.singleton "GREEN"), (2, BG.singleton "BLUE")]

  showsUndeclared =
    CEnum.showsWrappedUndeclared "Colour"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "Colour"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum Colour where

  minDeclaredValue = RED

  maxDeclaredValue = BLUE

instance Show Colour where

  showsPrec = CEnum.shows

instance Read Colour where

  readPrec = CEnum.readPrec

  readList = BG.readListDefault

  readListPrec = BG.readListPrecDefault

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapColour" Colour ty where

  hasField =
    \x0 ->
      (\y1 ->
         Colour {unwrapColour = y1}, BG.getField @"unwrapColour" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapColour" (BG.Ptr Colour) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapColour")

instance HasCField.HasCField Colour "unwrapColour" where

  type CFieldType Colour "unwrapColour" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RED@

    __defined at:__ @types\/boot\/ffi_module.h 37:15@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
pattern RED :: Colour
pattern RED = Colour 0

{-| __C declaration:__ @GREEN@

    __defined at:__ @types\/boot\/ffi_module.h 37:20@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
pattern GREEN :: Colour
pattern GREEN = Colour 1

{-| __C declaration:__ @BLUE@

    __defined at:__ @types\/boot\/ffi_module.h 37:27@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
pattern BLUE :: Colour
pattern BLUE = Colour 2

{-# COMPLETE RED, GREEN, BLUE #-}

{-| __C declaration:__ @union value@

    __defined at:__ @types\/boot\/ffi_module.h 40:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype Value = Value
  { unwrapValue :: BG.ByteArray
  }
  deriving stock (BG.Generic)

deriving via BG.SizedByteArray 4 4 instance Marshal.StaticSize Value

deriving via BG.SizedByteArray 4 4 instance Marshal.ReadRaw Value

deriving via BG.SizedByteArray 4 4 instance Marshal.WriteRaw Value

deriving via Marshal.EquivStorable Value instance BG.Storable Value

deriving via BG.SizedByteArray 4 4 instance Union.IsUnion Value

{-| __C declaration:__ @i@

    __defined at:__ @types\/boot\/ffi_module.h 41:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CInt) => BG.HasField "value_i" Value ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @i@

    __defined at:__ @types\/boot\/ffi_module.h 41:7@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "value_i" Value ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"value_i" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "value_i" (BG.Ptr Value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"value_i")

instance HasCField.HasCField Value "value_i" where

  type CFieldType Value "value_i" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @f@

    __defined at:__ @types\/boot\/ffi_module.h 42:9@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CFloat) => BG.HasField "value_f" Value ty where

  getField = BG.getUnionPayload

{-| __C declaration:__ @f@

    __defined at:__ @types\/boot\/ffi_module.h 42:9@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
instance (ty ~ BG.CFloat) => BG.CompatHasField.HasField "value_f" Value ty where

  hasField =
    \x0 ->
      (\y1 ->
         BG.setUnionPayload y1 x0, BG.getField @"value_f" x0)

instance ( ty ~ BG.CFloat
         ) => BG.HasField "value_f" (BG.Ptr Value) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"value_f")

instance HasCField.HasCField Value "value_f" where

  type CFieldType Value "value_f" = BG.CFloat

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @status@

    __defined at:__ @types\/boot\/ffi_module.h 47:13@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype Status = Status
  { unwrapStatus :: BG.CInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "unwrapStatus" Status ty where

  hasField =
    \x0 ->
      (\y1 ->
         Status {unwrapStatus = y1}, BG.getField @"unwrapStatus" x0)

instance ( ty ~ BG.CInt
         ) => BG.HasField "unwrapStatus" (BG.Ptr Status) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapStatus")

instance HasCField.HasCField Status "unwrapStatus" where

  type CFieldType Status "unwrapStatus" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @sample@

    __defined at:__ @types\/boot\/ffi_module.h 48:17@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype Sample = Sample
  { unwrapSample :: HsBindgen.Runtime.LibC.Int32
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.CompatHasField.HasField "unwrapSample" Sample ty where

  hasField =
    \x0 ->
      (\y1 ->
         Sample {unwrapSample = y1}, BG.getField @"unwrapSample" x0)

instance ( ty ~ HsBindgen.Runtime.LibC.Int32
         ) => BG.HasField "unwrapSample" (BG.Ptr Sample) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapSample")

instance HasCField.HasCField Sample "unwrapSample" where

  type CFieldType Sample "unwrapSample" =
    HsBindgen.Runtime.LibC.Int32

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro COUNT_T@

    __defined at:__ @types\/boot\/ffi_module.h 51:9@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
newtype COUNT_T = COUNT_T
  { unwrapCOUNT_T :: BG.CUInt
  }
  deriving stock (Eq, BG.Generic, Ord, Read, Show)
  deriving newtype
    ( BG.Bitfield
    , BG.Bits
    , Bounded
    , Enum
    , BG.FiniteBits
    , BG.HasFFIType
    , Integral
    , BG.Ix
    , Num
    , BG.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.CUInt
         ) => BG.CompatHasField.HasField "unwrapCOUNT_T" COUNT_T ty where

  hasField =
    \x0 ->
      (\y1 ->
         COUNT_T {unwrapCOUNT_T = y1}, BG.getField @"unwrapCOUNT_T" x0)

instance ( ty ~ BG.CUInt
         ) => BG.HasField "unwrapCOUNT_T" (BG.Ptr COUNT_T) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapCOUNT_T")

instance HasCField.HasCField COUNT_T "unwrapCOUNT_T" where

  type CFieldType COUNT_T "unwrapCOUNT_T" = BG.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @macro MAX_EVENTS@

    __defined at:__ @types\/boot\/ffi_module.h 52:9@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
mAX_EVENTS :: BG.CInt
mAX_EVENTS = (16 :: BG.CInt)

{-| __C declaration:__ @struct handle@

    __defined at:__ @types\/boot\/ffi_module.h 55:8@

    __exported by:__ @types\/boot\/ffi_module.h@
-}
data Handle

instance BG.ToFunPtr (BG.Ptr Point -> IO BG.CInt) where

  toFunPtr = hs_bindgen_ce6081d63db40b52

instance BG.FromFunPtr (BG.Ptr Point -> IO BG.CInt) where

  fromFunPtr = hs_bindgen_9ee31aa034cd5991
