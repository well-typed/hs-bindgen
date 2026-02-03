{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified Data.Bits as Bits
import qualified Data.Ix as Ix
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @struct Point@

    __defined at:__ @manual\/unprefixed_field_names.h 12:8@

    __exported by:__ @manual\/unprefixed_field_names.h@
-}
data Point = Point
  { x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/unprefixed_field_names.h 13:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
    -}
  , y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/unprefixed_field_names.h 14:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
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

    __defined at:__ @manual\/unprefixed_field_names.h 18:8@

    __exported by:__ @manual\/unprefixed_field_names.h@
-}
data Size = Size
  { width :: FC.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/unprefixed_field_names.h 19:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
    -}
  , height :: FC.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/unprefixed_field_names.h 20:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
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

    __defined at:__ @manual\/unprefixed_field_names.h 24:8@

    __exported by:__ @manual\/unprefixed_field_names.h@
-}
data Rect = Rect
  { x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/unprefixed_field_names.h 25:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
    -}
  , y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/unprefixed_field_names.h 26:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
    -}
  , width :: FC.CInt
    {- ^ __C declaration:__ @width@

         __defined at:__ @manual\/unprefixed_field_names.h 27:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
    -}
  , height :: FC.CInt
    {- ^ __C declaration:__ @height@

         __defined at:__ @manual\/unprefixed_field_names.h 28:7@

         __exported by:__ @manual\/unprefixed_field_names.h@
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

{-| __C declaration:__ @Value@

    __defined at:__ @manual\/unprefixed_field_names.h 32:13@

    __exported by:__ @manual\/unprefixed_field_names.h@
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
