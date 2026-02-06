{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
import qualified Data.Primitive.Types
import qualified Data.Proxy
import qualified Foreign as F
import qualified Foreign.C as FC
import qualified GHC.Generics
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.BitfieldPtr
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FLAM
import qualified HsBindgen.Runtime.HasCBitfield
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.Internal.Bitfield
import qualified HsBindgen.Runtime.Internal.ByteArray
import qualified HsBindgen.Runtime.Internal.HasFFIType
import qualified HsBindgen.Runtime.Internal.SizedByteArray
import qualified HsBindgen.Runtime.Marshal
import Data.Bits (FiniteBits)
import HsBindgen.Runtime.Internal.TypeEquality (TyEq)
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure)

{-| __C declaration:__ @struct point@

    __defined at:__ @manual\/zero_copy.h 12:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Point = Point
  { point_x :: FC.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/zero_copy.h 13:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , point_y :: FC.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/zero_copy.h 14:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Point where

  staticSizeOf = \_ -> (8 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point_x") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"point_y") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point_x") ptr0 point_x2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"point_y") ptr0 point_y3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Point instance F.Storable Point

instance HsBindgen.Runtime.HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "point_x")
         ) => GHC.Records.HasField "point_x" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point_x")

instance HsBindgen.Runtime.HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "point_y")
         ) => GHC.Records.HasField "point_y" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"point_y")

{-| __C declaration:__ @struct rectangle@

    __defined at:__ @manual\/zero_copy.h 17:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Rectangle = Rectangle
  { rectangle_topleft :: Point
    {- ^ __C declaration:__ @topleft@

         __defined at:__ @manual\/zero_copy.h 18:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , rectangle_bottomright :: Point
    {- ^ __C declaration:__ @bottomright@

         __defined at:__ @manual\/zero_copy.h 19:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Rectangle where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Rectangle where

  readRaw =
    \ptr0 ->
          pure Rectangle
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"rectangle_topleft") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"rectangle_bottomright") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Rectangle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle rectangle_topleft2 rectangle_bottomright3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"rectangle_topleft") ptr0 rectangle_topleft2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"rectangle_bottomright") ptr0 rectangle_bottomright3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Rectangle instance F.Storable Rectangle

instance HsBindgen.Runtime.HasCField.HasCField Rectangle "rectangle_topleft" where

  type CFieldType Rectangle "rectangle_topleft" = Point

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rectangle) "rectangle_topleft")
         ) => GHC.Records.HasField "rectangle_topleft" (Ptr.Ptr Rectangle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"rectangle_topleft")

instance HsBindgen.Runtime.HasCField.HasCField Rectangle "rectangle_bottomright" where

  type CFieldType Rectangle "rectangle_bottomright" =
    Point

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rectangle) "rectangle_bottomright")
         ) => GHC.Records.HasField "rectangle_bottomright" (Ptr.Ptr Rectangle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"rectangle_bottomright")

{-| __C declaration:__ @struct circle@

    __defined at:__ @manual\/zero_copy.h 22:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Circle = Circle
  { circle_midpoint :: Point
    {- ^ __C declaration:__ @midpoint@

         __defined at:__ @manual\/zero_copy.h 23:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , circle_radius :: FC.CInt
    {- ^ __C declaration:__ @radius@

         __defined at:__ @manual\/zero_copy.h 24:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Circle where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Circle where

  readRaw =
    \ptr0 ->
          pure Circle
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"circle_midpoint") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"circle_radius") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Circle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Circle circle_midpoint2 circle_radius3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"circle_midpoint") ptr0 circle_midpoint2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"circle_radius") ptr0 circle_radius3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Circle instance F.Storable Circle

instance HsBindgen.Runtime.HasCField.HasCField Circle "circle_midpoint" where

  type CFieldType Circle "circle_midpoint" = Point

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Circle) "circle_midpoint")
         ) => GHC.Records.HasField "circle_midpoint" (Ptr.Ptr Circle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"circle_midpoint")

instance HsBindgen.Runtime.HasCField.HasCField Circle "circle_radius" where

  type CFieldType Circle "circle_radius" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Circle) "circle_radius")
         ) => GHC.Records.HasField "circle_radius" (Ptr.Ptr Circle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"circle_radius")

{-| __C declaration:__ @union shape@

    __defined at:__ @manual\/zero_copy.h 30:7@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Shape = Shape
  { unwrapShape :: Data.Array.Byte.ByteArray
  }
  deriving stock (GHC.Generics.Generic)

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 4 instance HsBindgen.Runtime.Marshal.StaticSize Shape

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 4 instance HsBindgen.Runtime.Marshal.ReadRaw Shape

deriving via (HsBindgen.Runtime.Internal.SizedByteArray.SizedByteArray 16) 4 instance HsBindgen.Runtime.Marshal.WriteRaw Shape

deriving via HsBindgen.Runtime.Marshal.EquivStorable Shape instance F.Storable Shape

{-|

  __See:__ 'set_shape_rectangle'

__C declaration:__ @rectangle@

__defined at:__ @manual\/zero_copy.h 31:20@

__exported by:__ @manual\/zero_copy.h@
-}
get_shape_rectangle ::
     Shape
  -> Rectangle
get_shape_rectangle =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_shape_rectangle'

-}
set_shape_rectangle ::
     Rectangle
  -> Shape
set_shape_rectangle =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

{-|

  __See:__ 'set_shape_circle'

__C declaration:__ @circle@

__defined at:__ @manual\/zero_copy.h 32:17@

__exported by:__ @manual\/zero_copy.h@
-}
get_shape_circle ::
     Shape
  -> Circle
get_shape_circle =
  HsBindgen.Runtime.Internal.ByteArray.getUnionPayload

{-|

  __See:__ 'get_shape_circle'

-}
set_shape_circle ::
     Circle
  -> Shape
set_shape_circle =
  HsBindgen.Runtime.Internal.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Shape "shape_rectangle" where

  type CFieldType Shape "shape_rectangle" = Rectangle

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Shape) "shape_rectangle")
         ) => GHC.Records.HasField "shape_rectangle" (Ptr.Ptr Shape) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"shape_rectangle")

instance HsBindgen.Runtime.HasCField.HasCField Shape "shape_circle" where

  type CFieldType Shape "shape_circle" = Circle

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Shape) "shape_circle")
         ) => GHC.Records.HasField "shape_circle" (Ptr.Ptr Shape) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"shape_circle")

{-| __C declaration:__ @struct colour@

    __defined at:__ @manual\/zero_copy.h 39:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Colour = Colour
  { colour_opacity :: FC.CUInt
    {- ^ __C declaration:__ @opacity@

         __defined at:__ @manual\/zero_copy.h 40:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_brightness :: FC.CUInt
    {- ^ __C declaration:__ @brightness@

         __defined at:__ @manual\/zero_copy.h 41:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_red :: FC.CUInt
    {- ^ __C declaration:__ @red@

         __defined at:__ @manual\/zero_copy.h 42:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_green :: FC.CUInt
    {- ^ __C declaration:__ @green@

         __defined at:__ @manual\/zero_copy.h 43:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_blue :: FC.CUInt
    {- ^ __C declaration:__ @blue@

         __defined at:__ @manual\/zero_copy.h 44:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Colour where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Colour where

  readRaw =
    \ptr0 ->
          pure Colour
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"colour_opacity") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"colour_brightness") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"colour_red") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"colour_green") ptr0
      <*> HsBindgen.Runtime.HasCBitfield.peek (Data.Proxy.Proxy @"colour_blue") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Colour where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Colour
            colour_opacity2
            colour_brightness3
            colour_red4
            colour_green5
            colour_blue6 ->
                 HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"colour_opacity") ptr0 colour_opacity2
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"colour_brightness") ptr0 colour_brightness3
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"colour_red") ptr0 colour_red4
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"colour_green") ptr0 colour_green5
              >> HsBindgen.Runtime.HasCBitfield.poke (Data.Proxy.Proxy @"colour_blue") ptr0 colour_blue6

deriving via HsBindgen.Runtime.Marshal.EquivStorable Colour instance F.Storable Colour

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Colour "colour_opacity" where

  type CBitfieldType Colour "colour_opacity" = FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Colour) "colour_opacity")
         ) => GHC.Records.HasField "colour_opacity" (Ptr.Ptr Colour) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"colour_opacity")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Colour "colour_brightness" where

  type CBitfieldType Colour "colour_brightness" =
    FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 3

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Colour) "colour_brightness")
         ) => GHC.Records.HasField "colour_brightness" (Ptr.Ptr Colour) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"colour_brightness")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Colour "colour_red" where

  type CBitfieldType Colour "colour_red" = FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 5

  bitfieldWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Colour) "colour_red")
         ) => GHC.Records.HasField "colour_red" (Ptr.Ptr Colour) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"colour_red")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Colour "colour_green" where

  type CBitfieldType Colour "colour_green" = FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 13

  bitfieldWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Colour) "colour_green")
         ) => GHC.Records.HasField "colour_green" (Ptr.Ptr Colour) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"colour_green")

instance HsBindgen.Runtime.HasCBitfield.HasCBitfield Colour "colour_blue" where

  type CBitfieldType Colour "colour_blue" = FC.CUInt

  bitfieldOffset# = \_ -> \_ -> 21

  bitfieldWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCBitfield.CBitfieldType Colour) "colour_blue")
         ) => GHC.Records.HasField "colour_blue" (Ptr.Ptr Colour) (HsBindgen.Runtime.BitfieldPtr.BitfieldPtr ty) where

  getField =
    HsBindgen.Runtime.HasCBitfield.toPtr (Data.Proxy.Proxy @"colour_blue")

{-| __C declaration:__ @myInt@

    __defined at:__ @manual\/zero_copy.h 50:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
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

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyInt) "unwrapMyInt")
         ) => GHC.Records.HasField "unwrapMyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct drawing@

    __defined at:__ @manual\/zero_copy.h 55:16@

    __exported by:__ @manual\/zero_copy.h@
-}
data Drawing = Drawing
  { drawing_shape :: Ptr.Ptr Shape
    {- ^ __C declaration:__ @shape@

         __defined at:__ @manual\/zero_copy.h 56:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , drawing_colour :: Ptr.Ptr Colour
    {- ^ __C declaration:__ @colour@

         __defined at:__ @manual\/zero_copy.h 57:18@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Drawing where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Drawing where

  readRaw =
    \ptr0 ->
          pure Drawing
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"drawing_shape") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"drawing_colour") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Drawing where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Drawing drawing_shape2 drawing_colour3 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"drawing_shape") ptr0 drawing_shape2
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"drawing_colour") ptr0 drawing_colour3

deriving via HsBindgen.Runtime.Marshal.EquivStorable Drawing instance F.Storable Drawing

instance HsBindgen.Runtime.HasCField.HasCField Drawing "drawing_shape" where

  type CFieldType Drawing "drawing_shape" =
    Ptr.Ptr Shape

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Drawing) "drawing_shape")
         ) => GHC.Records.HasField "drawing_shape" (Ptr.Ptr Drawing) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"drawing_shape")

instance HsBindgen.Runtime.HasCField.HasCField Drawing "drawing_colour" where

  type CFieldType Drawing "drawing_colour" =
    Ptr.Ptr Colour

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Drawing) "drawing_colour")
         ) => GHC.Records.HasField "drawing_colour" (Ptr.Ptr Drawing) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"drawing_colour")

{-| __C declaration:__ @struct tic_tac_toe@

    __defined at:__ @manual\/zero_copy.h 63:16@

    __exported by:__ @manual\/zero_copy.h@
-}
data Tic_tac_toe = Tic_tac_toe
  { tic_tac_toe_row1 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @row1@

         __defined at:__ @manual\/zero_copy.h 64:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , tic_tac_toe_row2 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @row2@

         __defined at:__ @manual\/zero_copy.h 65:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , tic_tac_toe_row3 :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
    {- ^ __C declaration:__ @row3@

         __defined at:__ @manual\/zero_copy.h 66:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Tic_tac_toe where

  staticSizeOf = \_ -> (36 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Tic_tac_toe where

  readRaw =
    \ptr0 ->
          pure Tic_tac_toe
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"tic_tac_toe_row1") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"tic_tac_toe_row2") ptr0
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"tic_tac_toe_row3") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Tic_tac_toe where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tic_tac_toe tic_tac_toe_row12 tic_tac_toe_row23 tic_tac_toe_row34 ->
               HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"tic_tac_toe_row1") ptr0 tic_tac_toe_row12
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"tic_tac_toe_row2") ptr0 tic_tac_toe_row23
            >> HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"tic_tac_toe_row3") ptr0 tic_tac_toe_row34

deriving via HsBindgen.Runtime.Marshal.EquivStorable Tic_tac_toe instance F.Storable Tic_tac_toe

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row1" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row1" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row1")
         ) => GHC.Records.HasField "tic_tac_toe_row1" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"tic_tac_toe_row1")

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row2" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row2" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 12

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row2")
         ) => GHC.Records.HasField "tic_tac_toe_row2" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"tic_tac_toe_row2")

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row3" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row3" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row3")
         ) => GHC.Records.HasField "tic_tac_toe_row3" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"tic_tac_toe_row3")

{-| __C declaration:__ @struct vector@

    __defined at:__ @manual\/zero_copy.h 72:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Vector_Aux = Vector
  { vector_len :: FC.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @manual\/zero_copy.h 73:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)

instance HsBindgen.Runtime.Marshal.StaticSize Vector_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance HsBindgen.Runtime.Marshal.ReadRaw Vector_Aux where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.readRaw (Data.Proxy.Proxy @"vector_len") ptr0

instance HsBindgen.Runtime.Marshal.WriteRaw Vector_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_len2 ->
            HsBindgen.Runtime.HasCField.writeRaw (Data.Proxy.Proxy @"vector_len") ptr0 vector_len2

deriving via HsBindgen.Runtime.Marshal.EquivStorable Vector_Aux instance F.Storable Vector_Aux

instance HsBindgen.Runtime.HasCField.HasCField Vector_Aux "vector_len" where

  type CFieldType Vector_Aux "vector_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Vector_Aux) "vector_len")
         ) => GHC.Records.HasField "vector_len" (Ptr.Ptr Vector_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"vector_len")

instance HsBindgen.Runtime.FLAM.Offset FC.CChar Vector_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct vector@

    __defined at:__ @manual\/zero_copy.h 72:8@

    __exported by:__ @manual\/zero_copy.h@
-}
type Vector =
  (HsBindgen.Runtime.FLAM.WithFlam FC.CChar) Vector_Aux

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/zero_copy.h 82:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "unwrapTriplet")
         ) => GHC.Records.HasField "unwrapTriplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapTriplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/zero_copy.h 83:17@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (GHC.Generics.Generic)
  deriving stock (Eq, Show)
  deriving newtype
    ( HsBindgen.Runtime.Marshal.StaticSize
    , HsBindgen.Runtime.Marshal.ReadRaw
    , HsBindgen.Runtime.Marshal.WriteRaw
    , F.Storable
    )

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Matrix) "unwrapMatrix")
         ) => GHC.Records.HasField "unwrapMatrix" (Ptr.Ptr Matrix) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.fromPtr (Data.Proxy.Proxy @"unwrapMatrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0
