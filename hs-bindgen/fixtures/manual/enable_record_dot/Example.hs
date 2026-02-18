{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.CEnum as CEnum
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct Point@

    __defined at:__ @manual\/enable_record_dot.h 12:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Point = Point
  { x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/enable_record_dot.h 13:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/enable_record_dot.h 14:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HasCField.readRaw (RIP.Proxy @"x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"y") ptr0

instance Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point x2 y3 ->
               HasCField.writeRaw (RIP.Proxy @"x") ptr0 x2
            >> HasCField.writeRaw (RIP.Proxy @"y") ptr0 y3

deriving via Marshal.EquivStorable Point instance RIP.Storable Point

instance HasCField.HasCField Point "x" where

  type CFieldType Point "x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "x" (RIP.Ptr Point) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")

instance HasCField.HasCField Point "y" where

  type CFieldType Point "y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "y" (RIP.Ptr Point) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y")

{-| __C declaration:__ @struct Size@

    __defined at:__ @manual\/enable_record_dot.h 18:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Size = Size
  { width :: RIP.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/enable_record_dot.h 19:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , height :: RIP.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/enable_record_dot.h 20:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Size where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Size where

  readRaw =
    \ptr0 ->
          pure Size
      <*> HasCField.readRaw (RIP.Proxy @"width") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"height") ptr0

instance Marshal.WriteRaw Size where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Size width2 height3 ->
               HasCField.writeRaw (RIP.Proxy @"width") ptr0 width2
            >> HasCField.writeRaw (RIP.Proxy @"height") ptr0 height3

deriving via Marshal.EquivStorable Size instance RIP.Storable Size

instance HasCField.HasCField Size "width" where

  type CFieldType Size "width" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "width" (RIP.Ptr Size) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"width")

instance HasCField.HasCField Size "height" where

  type CFieldType Size "height" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "height" (RIP.Ptr Size) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"height")

{-| __C declaration:__ @struct Rect@

    __defined at:__ @manual\/enable_record_dot.h 24:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Rect = Rect
  { x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/enable_record_dot.h 25:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/enable_record_dot.h 26:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , width :: RIP.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/enable_record_dot.h 27:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , height :: RIP.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/enable_record_dot.h 28:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Rect where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rect where

  readRaw =
    \ptr0 ->
          pure Rect
      <*> HasCField.readRaw (RIP.Proxy @"x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"y") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"width") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"height") ptr0

instance Marshal.WriteRaw Rect where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect x2 y3 width4 height5 ->
               HasCField.writeRaw (RIP.Proxy @"x") ptr0 x2
            >> HasCField.writeRaw (RIP.Proxy @"y") ptr0 y3
            >> HasCField.writeRaw (RIP.Proxy @"width") ptr0 width4
            >> HasCField.writeRaw (RIP.Proxy @"height") ptr0 height5

deriving via Marshal.EquivStorable Rect instance RIP.Storable Rect

instance HasCField.HasCField Rect "x" where

  type CFieldType Rect "x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "x" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")

instance HasCField.HasCField Rect "y" where

  type CFieldType Rect "y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "y" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y")

instance HasCField.HasCField Rect "width" where

  type CFieldType Rect "width" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "width" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"width")

instance HasCField.HasCField Rect "height" where

  type CFieldType Rect "height" = RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "height" (RIP.Ptr Rect) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"height")

{-| __C declaration:__ @enum E@

    __defined at:__ @manual\/enable_record_dot.h 32:6@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype E = E
  { unwrap :: RIP.CUInt
  }
  deriving stock (Eq, RIP.Generic, Ord)
  deriving newtype (RIP.HasFFIType)

instance Marshal.StaticSize E where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> Marshal.readRawByteOff ptr0 (0 :: Int)

instance Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrap2 ->
            Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via Marshal.EquivStorable E instance RIP.Storable E

deriving via RIP.CUInt instance RIP.Prim E

instance CEnum.CEnum E where

  type CEnumZ E = RIP.CUInt

  toCEnum = E

  fromCEnum = RIP.getField @"unwrap"

  declaredValues =
    \_ ->
      CEnum.declaredValuesFromList [(0, RIP.singleton "X"), (1, RIP.singleton "Y")]

  showsUndeclared = CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    CEnum.readPrecWrappedUndeclared "E"

  isDeclared = CEnum.seqIsDeclared

  mkDeclared = CEnum.seqMkDeclared

instance CEnum.SequentialCEnum E where

  minDeclaredValue = X

  maxDeclaredValue = Y

instance Show E where

  showsPrec = CEnum.shows

instance Read E where

  readPrec = CEnum.readPrec

  readList = RIP.readListDefault

  readListPrec = RIP.readListPrecDefault

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "unwrap" (RIP.Ptr E) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField E "unwrap" where

  type CFieldType E "unwrap" = RIP.CUInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @x@

    __defined at:__ @manual\/enable_record_dot.h 33:3@

    __exported by:__ @manual\/enable_record_dot.h@
-}
pattern X :: E
pattern X = E 0

{-| __C declaration:__ @y@

    __defined at:__ @manual\/enable_record_dot.h 34:3@

    __exported by:__ @manual\/enable_record_dot.h@
-}
pattern Y :: E
pattern Y = E 1

{-| __C declaration:__ @Value@

    __defined at:__ @manual\/enable_record_dot.h 38:13@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype Value = Value
  { unwrap :: RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Ord, Read, Show)
  deriving newtype
    ( RIP.Bitfield
    , RIP.Bits
    , Bounded
    , Enum
    , RIP.FiniteBits
    , RIP.HasFFIType
    , Integral
    , RIP.Ix
    , Num
    , RIP.Prim
    , Marshal.ReadRaw
    , Real
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "unwrap" (RIP.Ptr Value) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField Value "unwrap" where

  type CFieldType Value "unwrap" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union U1@

    __defined at:__ @manual\/enable_record_dot.h 41:7@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U1 = U1
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize U1

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw U1

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw U1

deriving via Marshal.EquivStorable U1 instance RIP.Storable U1

{-|

  __See:__ 'set_u1_x'

__C declaration:__ @x@

__defined at:__ @manual\/enable_record_dot.h 42:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u1_x ::
     U1
  -> RIP.CInt
get_u1_x = RIP.getUnionPayload

{-|

  __See:__ 'get_u1_x'

-}
set_u1_x ::
     RIP.CInt
  -> U1
set_u1_x = RIP.setUnionPayload

{-|

  __See:__ 'set_u1_y'

__C declaration:__ @y@

__defined at:__ @manual\/enable_record_dot.h 43:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u1_y ::
     U1
  -> RIP.CInt
get_u1_y = RIP.getUnionPayload

{-|

  __See:__ 'get_u1_y'

-}
set_u1_y ::
     RIP.CInt
  -> U1
set_u1_y = RIP.setUnionPayload

instance HasCField.HasCField U1 "x" where

  type CFieldType U1 "x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (((~) ty) RIP.CInt) => RIP.HasField "x" (RIP.Ptr U1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")

instance HasCField.HasCField U1 "y" where

  type CFieldType U1 "y" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (((~) ty) RIP.CInt) => RIP.HasField "y" (RIP.Ptr U1) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y")

{-| __C declaration:__ @union U2@

    __defined at:__ @manual\/enable_record_dot.h 47:15@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U2_t = U2_t
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize U2_t

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw U2_t

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw U2_t

deriving via Marshal.EquivStorable U2_t instance RIP.Storable U2_t

{-|

  __See:__ 'set_u2_t_a'

__C declaration:__ @a@

__defined at:__ @manual\/enable_record_dot.h 48:8@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u2_t_a ::
     U2_t
  -> RIP.CChar
get_u2_t_a = RIP.getUnionPayload

{-|

  __See:__ 'get_u2_t_a'

-}
set_u2_t_a ::
     RIP.CChar
  -> U2_t
set_u2_t_a = RIP.setUnionPayload

{-|

  __See:__ 'set_u2_t_b'

__C declaration:__ @b@

__defined at:__ @manual\/enable_record_dot.h 49:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u2_t_b ::
     U2_t
  -> RIP.CInt
get_u2_t_b = RIP.getUnionPayload

{-|

  __See:__ 'get_u2_t_b'

-}
set_u2_t_b ::
     RIP.CInt
  -> U2_t
set_u2_t_b = RIP.setUnionPayload

instance HasCField.HasCField U2_t "a" where

  type CFieldType U2_t "a" = RIP.CChar

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CChar
         ) => RIP.HasField "a" (RIP.Ptr U2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"a")

instance HasCField.HasCField U2_t "b" where

  type CFieldType U2_t "b" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "b" (RIP.Ptr U2_t) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"b")

{-| __C declaration:__ @union U3@

    __defined at:__ @manual\/enable_record_dot.h 53:7@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U3 = U3
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.StaticSize U3

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.ReadRaw U3

deriving via (RIP.SizedByteArray 8) 4 instance Marshal.WriteRaw U3

deriving via Marshal.EquivStorable U3 instance RIP.Storable U3

{-|

  __See:__ 'set_u3_p'

__C declaration:__ @p@

__defined at:__ @manual\/enable_record_dot.h 54:16@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u3_p ::
     U3
  -> Point
get_u3_p = RIP.getUnionPayload

{-|

  __See:__ 'get_u3_p'

-}
set_u3_p ::
     Point
  -> U3
set_u3_p = RIP.setUnionPayload

{-|

  __See:__ 'set_u3_s'

__C declaration:__ @s@

__defined at:__ @manual\/enable_record_dot.h 55:15@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u3_s ::
     U3
  -> Size
get_u3_s = RIP.getUnionPayload

{-|

  __See:__ 'get_u3_s'

-}
set_u3_s ::
     Size
  -> U3
set_u3_s = RIP.setUnionPayload

instance HasCField.HasCField U3 "p" where

  type CFieldType U3 "p" = Point

  offset# = \_ -> \_ -> 0

instance (((~) ty) Point) => RIP.HasField "p" (RIP.Ptr U3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"p")

instance HasCField.HasCField U3 "s" where

  type CFieldType U3 "s" = Size

  offset# = \_ -> \_ -> 0

instance (((~) ty) Size) => RIP.HasField "s" (RIP.Ptr U3) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"s")

{-| __C declaration:__ @union U4@

    __defined at:__ @manual\/enable_record_dot.h 59:7@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U4 = U4
  { unwrap :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.StaticSize U4

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.ReadRaw U4

deriving via (RIP.SizedByteArray 4) 4 instance Marshal.WriteRaw U4

deriving via Marshal.EquivStorable U4 instance RIP.Storable U4

{-|

  __See:__ 'set_u4_x'

__C declaration:__ @x@

__defined at:__ @manual\/enable_record_dot.h 60:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u4_x ::
     U4
  -> RIP.CInt
get_u4_x = RIP.getUnionPayload

{-|

  __See:__ 'get_u4_x'

-}
set_u4_x ::
     RIP.CInt
  -> U4
set_u4_x = RIP.setUnionPayload

{-|

  __See:__ 'set_u4_y'

__C declaration:__ @y@

__defined at:__ @manual\/enable_record_dot.h 61:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_u4_y ::
     U4
  -> RIP.CInt
get_u4_y = RIP.getUnionPayload

{-|

  __See:__ 'get_u4_y'

-}
set_u4_y ::
     RIP.CInt
  -> U4
set_u4_y = RIP.setUnionPayload

instance HasCField.HasCField U4 "x" where

  type CFieldType U4 "x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (((~) ty) RIP.CInt) => RIP.HasField "x" (RIP.Ptr U4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"x")

instance HasCField.HasCField U4 "y" where

  type CFieldType U4 "y" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance (((~) ty) RIP.CInt) => RIP.HasField "y" (RIP.Ptr U4) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"y")

{-| __C declaration:__ @struct Driver@

    __defined at:__ @manual\/enable_record_dot.h 65:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Driver

{-| Auxiliary type used by 'RunDriver'

__C declaration:__ @RunDriver@

__defined at:__ @manual\/enable_record_dot.h 66:15@

__exported by:__ @manual\/enable_record_dot.h@
-}
newtype RunDriver_Aux = RunDriver_Aux
  { unwrap :: (RIP.Ptr Driver) -> IO RIP.CInt
  }
  deriving stock (RIP.Generic)
  deriving newtype (RIP.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> IO (RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (RIP.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    fmap RIP.castFunPtrFromFFIType (hs_bindgen_d86ecf261d7044c6_base (RIP.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     RIP.FunPtr ((RIP.Ptr RIP.Void) -> IO RIP.Int32)
  -> (RIP.Ptr RIP.Void) -> IO RIP.Int32

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     RIP.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    RIP.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (RIP.castFunPtrToFFIType funPtr0))

instance RIP.ToFunPtr RunDriver_Aux where

  toFunPtr = hs_bindgen_d86ecf261d7044c6

instance RIP.FromFunPtr RunDriver_Aux where

  fromFunPtr = hs_bindgen_6520ae39b50ffb4e

instance ( ((~) ty) ((RIP.Ptr Driver) -> IO RIP.CInt)
         ) => RIP.HasField "unwrap" (RIP.Ptr RunDriver_Aux) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField RunDriver_Aux "unwrap" where

  type CFieldType RunDriver_Aux "unwrap" =
    (RIP.Ptr Driver) -> IO RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RunDriver@

    __defined at:__ @manual\/enable_record_dot.h 66:15@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype RunDriver = RunDriver
  { unwrap :: RIP.FunPtr RunDriver_Aux
  }
  deriving stock (Eq, RIP.Generic, Ord, Show)
  deriving newtype
    ( RIP.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) (RIP.FunPtr RunDriver_Aux)
         ) => RIP.HasField "unwrap" (RIP.Ptr RunDriver) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"unwrap")

instance HasCField.HasCField RunDriver "unwrap" where

  type CFieldType RunDriver "unwrap" =
    RIP.FunPtr RunDriver_Aux

  offset# = \_ -> \_ -> 0
