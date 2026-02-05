{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
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

module Example where

import qualified Data.Array.Byte
import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.List.NonEmpty
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Int
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.CEnum
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.FunPtr
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import qualified Prelude as P
import qualified Text.Read
import Data.Bits (FiniteBits)
import Data.Void (Void)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, IO, Int, Integral, Num, Ord, Read, Real, Show, pure, showsPrec)

{-| __C declaration:__ @struct Point@

    __defined at:__ @manual\/enable_record_dot.h 12:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Point = Point
  { x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/enable_record_dot.h 13:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/enable_record_dot.h 14:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point x2 y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"x") ptr0 x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"y") ptr0 y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Point instance F.Storable Point

instance HsBindgen.Runtime.HasCField.HasCField Point "x" where

  type CFieldType Point "x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "x")
         ) => GHC.Records.HasField "x" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"x")

instance HsBindgen.Runtime.HasCField.HasCField Point "y" where

  type CFieldType Point "y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "y")
         ) => GHC.Records.HasField "y" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"y")

{-| __C declaration:__ @struct Size@

    __defined at:__ @manual\/enable_record_dot.h 18:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Size = Size
  { width :: FC.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/enable_record_dot.h 19:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , height :: FC.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/enable_record_dot.h 20:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Size where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Size where

  readRaw =
    \ptr0 ->
          pure Size
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"width") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"height") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Size where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Size width2 height3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"width") ptr0 width2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"height") ptr0 height3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Size instance F.Storable Size

instance HsBindgen.Runtime.HasCField.HasCField Size "width" where

  type CFieldType Size "width" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Size) "width")
         ) => GHC.Records.HasField "width" (Ptr.Ptr Size) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"width")

instance HsBindgen.Runtime.HasCField.HasCField Size "height" where

  type CFieldType Size "height" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Size) "height")
         ) => GHC.Records.HasField "height" (Ptr.Ptr Size) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"height")

{-| __C declaration:__ @struct Rect@

    __defined at:__ @manual\/enable_record_dot.h 24:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Rect = Rect
  { x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/enable_record_dot.h 25:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/enable_record_dot.h 26:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , width :: FC.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/enable_record_dot.h 27:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  , height :: FC.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/enable_record_dot.h 28:7@

         __exported by:__ @manual\/enable_record_dot.h@
    -}
  }
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Rect where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Rect where

  readRaw =
    \ptr0 ->
          pure Rect
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"y") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"width") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"height") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Rect where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rect x2 y3 width4 height5 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"x") ptr0 x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"y") ptr0 y3
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"width") ptr0 width4
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"height") ptr0 height5

deriving via HsBindgen.Runtime.Marshal.EquivStorable Rect instance F.Storable Rect

instance HsBindgen.Runtime.HasCField.HasCField Rect "x" where

  type CFieldType Rect "x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rect) "x")
         ) => GHC.Records.HasField "x" (Ptr.Ptr Rect) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"x")

instance HsBindgen.Runtime.HasCField.HasCField Rect "y" where

  type CFieldType Rect "y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rect) "y")
         ) => GHC.Records.HasField "y" (Ptr.Ptr Rect) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"y")

instance HsBindgen.Runtime.HasCField.HasCField Rect "width" where

  type CFieldType Rect "width" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rect) "width")
         ) => GHC.Records.HasField "width" (Ptr.Ptr Rect) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"width")

instance HsBindgen.Runtime.HasCField.HasCField Rect "height" where

  type CFieldType Rect "height" = FC.CInt

  offset# = \_ -> \_ -> 12

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rect) "height")
         ) => GHC.Records.HasField "height" (Ptr.Ptr Rect) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"height")

{-| __C declaration:__ @enum E@

    __defined at:__ @manual\/enable_record_dot.h 32:6@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype E = E
  { unwrap :: FC.CUInt
  }
  deriving stock (Eq, Ord)
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

instance HsBindgen.Runtime.Marshal.StaticSize E where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw E where

  readRaw =
    \ptr0 ->
          pure E
      <*> HsBindgen.Runtime.Marshal.readRawByteOff ptr0 (0 :: Int)

instance HsBindgen.Runtime.Marshal.WriteRaw E where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          E unwrap2 ->
            HsBindgen.Runtime.Marshal.writeRawByteOff ptr0 (0 :: Int) unwrap2

deriving via HsBindgen.Runtime.Marshal.EquivStorable E instance F.Storable E

deriving via FC.CUInt instance Data.Primitive.Types.Prim E

instance HsBindgen.Runtime.CEnum.CEnum E where

  type CEnumZ E = FC.CUInt

  toCEnum = E

  fromCEnum = GHC.Records.getField @"unwrap"

  declaredValues =
    \_ ->
      HsBindgen.Runtime.CEnum.declaredValuesFromList [(0, Data.List.NonEmpty.singleton "X"), (1, Data.List.NonEmpty.singleton "Y")]

  showsUndeclared =
    HsBindgen.Runtime.CEnum.showsWrappedUndeclared "E"

  readPrecUndeclared =
    HsBindgen.Runtime.CEnum.readPrecWrappedUndeclared "E"

  isDeclared = HsBindgen.Runtime.CEnum.seqIsDeclared

  mkDeclared = HsBindgen.Runtime.CEnum.seqMkDeclared

instance HsBindgen.Runtime.CEnum.SequentialCEnum E where

  minDeclaredValue = X

  maxDeclaredValue = Y

instance Show E where

  showsPrec = HsBindgen.Runtime.CEnum.shows

instance Read E where

  readPrec = HsBindgen.Runtime.CEnum.readPrec

  readList = Text.Read.readListDefault

  readListPrec = Text.Read.readListPrecDefault

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType E) "unwrap")
         ) => GHC.Records.HasField "unwrap" (Ptr.Ptr E) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField E "unwrap" where

  type CFieldType E "unwrap" = FC.CUInt

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
  { unwrap :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    , Data.Primitive.Types.Prim
    , HsBindgen.Runtime.Internal.Bitfield.Bitfield
    , Bits.Bits
    , Bounded
    , Enum
    , FiniteBits
    , Integral
    , Ix.Ix
    , Num
    , Real
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Value) "unwrap")
         ) => GHC.Records.HasField "unwrap" (Ptr.Ptr Value) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField Value "unwrap" where

  type CFieldType Value "unwrap" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @union U1@

    __defined at:__ @manual\/enable_record_dot.h 41:7@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U1 = U1
  { unwrap :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize U1

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw U1

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw U1

deriving via HsBindgen.Runtime.Marshal.EquivStorable U1 instance F.Storable U1

{-|

  __See:__ 'set_x'

__C declaration:__ @x@

__defined at:__ @manual\/enable_record_dot.h 42:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_x ::
     U1
  -> FC.CInt
get_x =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_x'

-}
set_x ::
     FC.CInt
  -> U1
set_x =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_y'

__C declaration:__ @y@

__defined at:__ @manual\/enable_record_dot.h 43:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_y ::
     U1
  -> FC.CInt
get_y =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_y'

-}
set_y ::
     FC.CInt
  -> U1
set_y =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField U1 "x" where

  type CFieldType U1 "x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U1) "x")
         ) => GHC.Records.HasField "x" (Ptr.Ptr U1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"x")

instance HsBindgen.Runtime.HasCField.HasCField U1 "y" where

  type CFieldType U1 "y" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U1) "y")
         ) => GHC.Records.HasField "y" (Ptr.Ptr U1) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"y")

{-| __C declaration:__ @union U2@

    __defined at:__ @manual\/enable_record_dot.h 47:15@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U2_t = U2_t
  { unwrap :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.StaticSize U2_t

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.ReadRaw U2_t

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 4) 4 instance HsBindgen.Runtime.Marshal.WriteRaw U2_t

deriving via HsBindgen.Runtime.Marshal.EquivStorable U2_t instance F.Storable U2_t

{-|

  __See:__ 'set_a'

__C declaration:__ @a@

__defined at:__ @manual\/enable_record_dot.h 48:8@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_a ::
     U2_t
  -> FC.CChar
get_a =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_a'

-}
set_a ::
     FC.CChar
  -> U2_t
set_a =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_b'

__C declaration:__ @b@

__defined at:__ @manual\/enable_record_dot.h 49:7@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_b ::
     U2_t
  -> FC.CInt
get_b =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_b'

-}
set_b ::
     FC.CInt
  -> U2_t
set_b =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField U2_t "a" where

  type CFieldType U2_t "a" = FC.CChar

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U2_t) "a")
         ) => GHC.Records.HasField "a" (Ptr.Ptr U2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"a")

instance HsBindgen.Runtime.HasCField.HasCField U2_t "b" where

  type CFieldType U2_t "b" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U2_t) "b")
         ) => GHC.Records.HasField "b" (Ptr.Ptr U2_t) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"b")

{-| __C declaration:__ @union U3@

    __defined at:__ @manual\/enable_record_dot.h 53:7@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype U3 = U3
  { unwrap :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.StaticSize U3

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.ReadRaw U3

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 8) 4 instance HsBindgen.Runtime.Marshal.WriteRaw U3

deriving via HsBindgen.Runtime.Marshal.EquivStorable U3 instance F.Storable U3

{-|

  __See:__ 'set_p'

__C declaration:__ @p@

__defined at:__ @manual\/enable_record_dot.h 54:16@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_p ::
     U3
  -> Point
get_p =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_p'

-}
set_p ::
     Point
  -> U3
set_p =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_s'

__C declaration:__ @s@

__defined at:__ @manual\/enable_record_dot.h 55:15@

__exported by:__ @manual\/enable_record_dot.h@
-}
get_s ::
     U3
  -> Size
get_s =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_s'

-}
set_s ::
     Size
  -> U3
set_s =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField U3 "p" where

  type CFieldType U3 "p" = Point

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U3) "p")
         ) => GHC.Records.HasField "p" (Ptr.Ptr U3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"p")

instance HsBindgen.Runtime.HasCField.HasCField U3 "s" where

  type CFieldType U3 "s" = Size

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType U3) "s")
         ) => GHC.Records.HasField "s" (Ptr.Ptr U3) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"s")

{-| __C declaration:__ @struct Driver@

    __defined at:__ @manual\/enable_record_dot.h 59:8@

    __exported by:__ @manual\/enable_record_dot.h@
-}
data Driver

{-| Auxiliary type used by 'RunDriver'

__C declaration:__ @RunDriver@

__defined at:__ @manual\/enable_record_dot.h 60:15@

__exported by:__ @manual\/enable_record_dot.h@
-}
newtype RunDriver_Aux = RunDriver_Aux
  { unwrap :: (Ptr.Ptr Driver) -> IO FC.CInt
  }
  deriving newtype (HsBindgen.Runtime.Internal.HasFFIType.HasFFIType)

foreign import ccall safe "wrapper" hs_bindgen_d86ecf261d7044c6_base ::
     ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> IO (Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32))

-- __unique:__ @toRunDriver_Aux@
hs_bindgen_d86ecf261d7044c6 ::
     RunDriver_Aux
  -> IO (Ptr.FunPtr RunDriver_Aux)
hs_bindgen_d86ecf261d7044c6 =
  \fun0 ->
    P.fmap HsBindgen.Runtime.Internal.HasFFIType.castFunPtrFromFFIType (hs_bindgen_d86ecf261d7044c6_base (HsBindgen.Runtime.Internal.HasFFIType.toFFIType fun0))

foreign import ccall safe "dynamic" hs_bindgen_6520ae39b50ffb4e_base ::
     Ptr.FunPtr ((Ptr.Ptr Void) -> IO GHC.Int.Int32)
  -> (Ptr.Ptr Void) -> IO GHC.Int.Int32

-- __unique:__ @fromRunDriver_Aux@
hs_bindgen_6520ae39b50ffb4e ::
     Ptr.FunPtr RunDriver_Aux
  -> RunDriver_Aux
hs_bindgen_6520ae39b50ffb4e =
  \funPtr0 ->
    HsBindgen.Runtime.Internal.HasFFIType.fromFFIType (hs_bindgen_6520ae39b50ffb4e_base (HsBindgen.Runtime.Internal.HasFFIType.castFunPtrToFFIType funPtr0))

instance HsBindgen.Runtime.Internal.FunPtr.ToFunPtr RunDriver_Aux where

  toFunPtr = hs_bindgen_d86ecf261d7044c6

instance HsBindgen.Runtime.Internal.FunPtr.FromFunPtr RunDriver_Aux where

  fromFunPtr = hs_bindgen_6520ae39b50ffb4e

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType RunDriver_Aux) "unwrap")
         ) => GHC.Records.HasField "unwrap" (Ptr.Ptr RunDriver_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField RunDriver_Aux "unwrap" where

  type CFieldType RunDriver_Aux "unwrap" =
    (Ptr.Ptr Driver) -> IO FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @RunDriver@

    __defined at:__ @manual\/enable_record_dot.h 60:15@

    __exported by:__ @manual\/enable_record_dot.h@
-}
newtype RunDriver = RunDriver
  { unwrap :: Ptr.FunPtr RunDriver_Aux
  }
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    , HsBindgen.Runtime.Internal.HasFFIType.HasFFIType
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType RunDriver) "unwrap")
         ) => GHC.Records.HasField "unwrap" (Ptr.Ptr RunDriver) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrap")

instance HsBindgen.Runtime.HasCField.HasCField RunDriver "unwrap" where

  type CFieldType RunDriver "unwrap" =
    Ptr.FunPtr RunDriver_Aux

  offset# = \_ -> \_ -> 0
