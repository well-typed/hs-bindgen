{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Example where

import qualified HsBindgen.Runtime.BitfieldPtr as BitfieldPtr
import qualified HsBindgen.Runtime.ConstantArray as CA
import qualified HsBindgen.Runtime.FLAM as FLAM
import qualified HsBindgen.Runtime.HasCBitfield as HasCBitfield
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Internal.Prelude as RIP
import qualified HsBindgen.Runtime.Marshal as Marshal

{-| __C declaration:__ @struct point@

    __defined at:__ @manual\/zero_copy.h 12:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Point = Point
  { point_x :: RIP.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @manual\/zero_copy.h 13:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , point_y :: RIP.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @manual\/zero_copy.h 14:7@

         __exported by:__ @manual\/zero_copy.h@
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
      <*> HasCField.readRaw (RIP.Proxy @"point_x") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"point_y") ptr0

instance Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 ->
               HasCField.writeRaw (RIP.Proxy @"point_x") ptr0 point_x2
            >> HasCField.writeRaw (RIP.Proxy @"point_y") ptr0 point_y3

deriving via Marshal.EquivStorable Point instance RIP.Storable Point

instance HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point_x" (RIP.Ptr Point) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point_x")

instance HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = RIP.CInt

  offset# = \_ -> \_ -> 4

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "point_y" (RIP.Ptr Point) (RIP.Ptr ty) where

  getField = HasCField.fromPtr (RIP.Proxy @"point_y")

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
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Rectangle where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Rectangle where

  readRaw =
    \ptr0 ->
          pure Rectangle
      <*> HasCField.readRaw (RIP.Proxy @"rectangle_topleft") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"rectangle_bottomright") ptr0

instance Marshal.WriteRaw Rectangle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle rectangle_topleft2 rectangle_bottomright3 ->
               HasCField.writeRaw (RIP.Proxy @"rectangle_topleft") ptr0 rectangle_topleft2
            >> HasCField.writeRaw (RIP.Proxy @"rectangle_bottomright") ptr0 rectangle_bottomright3

deriving via Marshal.EquivStorable Rectangle instance RIP.Storable Rectangle

instance HasCField.HasCField Rectangle "rectangle_topleft" where

  type CFieldType Rectangle "rectangle_topleft" = Point

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Point
         ) => RIP.HasField "rectangle_topleft" (RIP.Ptr Rectangle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"rectangle_topleft")

instance HasCField.HasCField Rectangle "rectangle_bottomright" where

  type CFieldType Rectangle "rectangle_bottomright" =
    Point

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) Point
         ) => RIP.HasField "rectangle_bottomright" (RIP.Ptr Rectangle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"rectangle_bottomright")

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
  , circle_radius :: RIP.CInt
    {- ^ __C declaration:__ @radius@

         __defined at:__ @manual\/zero_copy.h 24:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Circle where

  staticSizeOf = \_ -> (12 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Circle where

  readRaw =
    \ptr0 ->
          pure Circle
      <*> HasCField.readRaw (RIP.Proxy @"circle_midpoint") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"circle_radius") ptr0

instance Marshal.WriteRaw Circle where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Circle circle_midpoint2 circle_radius3 ->
               HasCField.writeRaw (RIP.Proxy @"circle_midpoint") ptr0 circle_midpoint2
            >> HasCField.writeRaw (RIP.Proxy @"circle_radius") ptr0 circle_radius3

deriving via Marshal.EquivStorable Circle instance RIP.Storable Circle

instance HasCField.HasCField Circle "circle_midpoint" where

  type CFieldType Circle "circle_midpoint" = Point

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Point
         ) => RIP.HasField "circle_midpoint" (RIP.Ptr Circle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"circle_midpoint")

instance HasCField.HasCField Circle "circle_radius" where

  type CFieldType Circle "circle_radius" = RIP.CInt

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "circle_radius" (RIP.Ptr Circle) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"circle_radius")

{-| __C declaration:__ @union shape@

    __defined at:__ @manual\/zero_copy.h 30:7@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Shape = Shape
  { unwrapShape :: RIP.ByteArray
  }
  deriving stock (RIP.Generic)

deriving via (RIP.SizedByteArray 16) 4 instance Marshal.StaticSize Shape

deriving via (RIP.SizedByteArray 16) 4 instance Marshal.ReadRaw Shape

deriving via (RIP.SizedByteArray 16) 4 instance Marshal.WriteRaw Shape

deriving via Marshal.EquivStorable Shape instance RIP.Storable Shape

{-|

  __See:__ 'set_shape_rectangle'

__C declaration:__ @rectangle@

__defined at:__ @manual\/zero_copy.h 31:20@

__exported by:__ @manual\/zero_copy.h@
-}
get_shape_rectangle ::
     Shape
  -> Rectangle
get_shape_rectangle = RIP.getUnionPayload

{-|

  __See:__ 'get_shape_rectangle'

-}
set_shape_rectangle ::
     Rectangle
  -> Shape
set_shape_rectangle = RIP.setUnionPayload

{-|

  __See:__ 'set_shape_circle'

__C declaration:__ @circle@

__defined at:__ @manual\/zero_copy.h 32:17@

__exported by:__ @manual\/zero_copy.h@
-}
get_shape_circle ::
     Shape
  -> Circle
get_shape_circle = RIP.getUnionPayload

{-|

  __See:__ 'get_shape_circle'

-}
set_shape_circle ::
     Circle
  -> Shape
set_shape_circle = RIP.setUnionPayload

instance HasCField.HasCField Shape "shape_rectangle" where

  type CFieldType Shape "shape_rectangle" = Rectangle

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Rectangle
         ) => RIP.HasField "shape_rectangle" (RIP.Ptr Shape) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"shape_rectangle")

instance HasCField.HasCField Shape "shape_circle" where

  type CFieldType Shape "shape_circle" = Circle

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) Circle
         ) => RIP.HasField "shape_circle" (RIP.Ptr Shape) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"shape_circle")

{-| __C declaration:__ @struct colour@

    __defined at:__ @manual\/zero_copy.h 39:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Colour = Colour
  { colour_opacity :: RIP.CUInt
    {- ^ __C declaration:__ @opacity@

         __defined at:__ @manual\/zero_copy.h 40:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_brightness :: RIP.CUInt
    {- ^ __C declaration:__ @brightness@

         __defined at:__ @manual\/zero_copy.h 41:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_red :: RIP.CUInt
    {- ^ __C declaration:__ @red@

         __defined at:__ @manual\/zero_copy.h 42:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_green :: RIP.CUInt
    {- ^ __C declaration:__ @green@

         __defined at:__ @manual\/zero_copy.h 43:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , colour_blue :: RIP.CUInt
    {- ^ __C declaration:__ @blue@

         __defined at:__ @manual\/zero_copy.h 44:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Colour where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Colour where

  readRaw =
    \ptr0 ->
          pure Colour
      <*> HasCBitfield.peek (RIP.Proxy @"colour_opacity") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"colour_brightness") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"colour_red") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"colour_green") ptr0
      <*> HasCBitfield.peek (RIP.Proxy @"colour_blue") ptr0

instance Marshal.WriteRaw Colour where

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
                 HasCBitfield.poke (RIP.Proxy @"colour_opacity") ptr0 colour_opacity2
              >> HasCBitfield.poke (RIP.Proxy @"colour_brightness") ptr0 colour_brightness3
              >> HasCBitfield.poke (RIP.Proxy @"colour_red") ptr0 colour_red4
              >> HasCBitfield.poke (RIP.Proxy @"colour_green") ptr0 colour_green5
              >> HasCBitfield.poke (RIP.Proxy @"colour_blue") ptr0 colour_blue6

deriving via Marshal.EquivStorable Colour instance RIP.Storable Colour

instance HasCBitfield.HasCBitfield Colour "colour_opacity" where

  type CBitfieldType Colour "colour_opacity" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 0

  bitfieldWidth# = \_ -> \_ -> 2

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "colour_opacity" (RIP.Ptr Colour) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"colour_opacity")

instance HasCBitfield.HasCBitfield Colour "colour_brightness" where

  type CBitfieldType Colour "colour_brightness" =
    RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 2

  bitfieldWidth# = \_ -> \_ -> 3

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "colour_brightness" (RIP.Ptr Colour) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"colour_brightness")

instance HasCBitfield.HasCBitfield Colour "colour_red" where

  type CBitfieldType Colour "colour_red" = RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 5

  bitfieldWidth# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "colour_red" (RIP.Ptr Colour) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"colour_red")

instance HasCBitfield.HasCBitfield Colour "colour_green" where

  type CBitfieldType Colour "colour_green" = RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 13

  bitfieldWidth# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "colour_green" (RIP.Ptr Colour) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"colour_green")

instance HasCBitfield.HasCBitfield Colour "colour_blue" where

  type CBitfieldType Colour "colour_blue" = RIP.CUInt

  bitfieldOffset# = \_ -> \_ -> 21

  bitfieldWidth# = \_ -> \_ -> 8

instance ( ((~) ty) RIP.CUInt
         ) => RIP.HasField "colour_blue" (RIP.Ptr Colour) (BitfieldPtr.BitfieldPtr ty) where

  getField =
    HasCBitfield.toPtr (RIP.Proxy @"colour_blue")

{-| __C declaration:__ @myInt@

    __defined at:__ @manual\/zero_copy.h 50:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype MyInt = MyInt
  { unwrapMyInt :: RIP.CInt
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
         ) => RIP.HasField "unwrapMyInt" (RIP.Ptr MyInt) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMyInt")

instance HasCField.HasCField MyInt "unwrapMyInt" where

  type CFieldType MyInt "unwrapMyInt" = RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @struct drawing@

    __defined at:__ @manual\/zero_copy.h 55:16@

    __exported by:__ @manual\/zero_copy.h@
-}
data Drawing = Drawing
  { drawing_shape :: RIP.Ptr Shape
    {- ^ __C declaration:__ @shape@

         __defined at:__ @manual\/zero_copy.h 56:16@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , drawing_colour :: RIP.Ptr Colour
    {- ^ __C declaration:__ @colour@

         __defined at:__ @manual\/zero_copy.h 57:18@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Drawing where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Drawing where

  readRaw =
    \ptr0 ->
          pure Drawing
      <*> HasCField.readRaw (RIP.Proxy @"drawing_shape") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"drawing_colour") ptr0

instance Marshal.WriteRaw Drawing where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Drawing drawing_shape2 drawing_colour3 ->
               HasCField.writeRaw (RIP.Proxy @"drawing_shape") ptr0 drawing_shape2
            >> HasCField.writeRaw (RIP.Proxy @"drawing_colour") ptr0 drawing_colour3

deriving via Marshal.EquivStorable Drawing instance RIP.Storable Drawing

instance HasCField.HasCField Drawing "drawing_shape" where

  type CFieldType Drawing "drawing_shape" =
    RIP.Ptr Shape

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) (RIP.Ptr Shape)
         ) => RIP.HasField "drawing_shape" (RIP.Ptr Drawing) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"drawing_shape")

instance HasCField.HasCField Drawing "drawing_colour" where

  type CFieldType Drawing "drawing_colour" =
    RIP.Ptr Colour

  offset# = \_ -> \_ -> 8

instance ( ((~) ty) (RIP.Ptr Colour)
         ) => RIP.HasField "drawing_colour" (RIP.Ptr Drawing) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"drawing_colour")

{-| __C declaration:__ @struct tic_tac_toe@

    __defined at:__ @manual\/zero_copy.h 63:16@

    __exported by:__ @manual\/zero_copy.h@
-}
data Tic_tac_toe = Tic_tac_toe
  { tic_tac_toe_row1 :: (CA.ConstantArray 3) RIP.CInt
    {- ^ __C declaration:__ @row1@

         __defined at:__ @manual\/zero_copy.h 64:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , tic_tac_toe_row2 :: (CA.ConstantArray 3) RIP.CInt
    {- ^ __C declaration:__ @row2@

         __defined at:__ @manual\/zero_copy.h 65:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  , tic_tac_toe_row3 :: (CA.ConstantArray 3) RIP.CInt
    {- ^ __C declaration:__ @row3@

         __defined at:__ @manual\/zero_copy.h 66:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Tic_tac_toe where

  staticSizeOf = \_ -> (36 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Tic_tac_toe where

  readRaw =
    \ptr0 ->
          pure Tic_tac_toe
      <*> HasCField.readRaw (RIP.Proxy @"tic_tac_toe_row1") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"tic_tac_toe_row2") ptr0
      <*> HasCField.readRaw (RIP.Proxy @"tic_tac_toe_row3") ptr0

instance Marshal.WriteRaw Tic_tac_toe where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tic_tac_toe tic_tac_toe_row12 tic_tac_toe_row23 tic_tac_toe_row34 ->
               HasCField.writeRaw (RIP.Proxy @"tic_tac_toe_row1") ptr0 tic_tac_toe_row12
            >> HasCField.writeRaw (RIP.Proxy @"tic_tac_toe_row2") ptr0 tic_tac_toe_row23
            >> HasCField.writeRaw (RIP.Proxy @"tic_tac_toe_row3") ptr0 tic_tac_toe_row34

deriving via Marshal.EquivStorable Tic_tac_toe instance RIP.Storable Tic_tac_toe

instance HasCField.HasCField Tic_tac_toe "tic_tac_toe_row1" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row1" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "tic_tac_toe_row1" (RIP.Ptr Tic_tac_toe) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"tic_tac_toe_row1")

instance HasCField.HasCField Tic_tac_toe "tic_tac_toe_row2" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row2" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 12

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "tic_tac_toe_row2" (RIP.Ptr Tic_tac_toe) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"tic_tac_toe_row2")

instance HasCField.HasCField Tic_tac_toe "tic_tac_toe_row3" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row3" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 24

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "tic_tac_toe_row3" (RIP.Ptr Tic_tac_toe) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"tic_tac_toe_row3")

{-| __C declaration:__ @struct vector@

    __defined at:__ @manual\/zero_copy.h 72:8@

    __exported by:__ @manual\/zero_copy.h@
-}
data Vector_Aux = Vector
  { vector_len :: RIP.CInt
    {- ^ __C declaration:__ @len@

         __defined at:__ @manual\/zero_copy.h 73:7@

         __exported by:__ @manual\/zero_copy.h@
    -}
  }
  deriving stock (Eq, RIP.Generic, Show)

instance Marshal.StaticSize Vector_Aux where

  staticSizeOf = \_ -> (4 :: Int)

  staticAlignment = \_ -> (4 :: Int)

instance Marshal.ReadRaw Vector_Aux where

  readRaw =
    \ptr0 ->
          pure Vector
      <*> HasCField.readRaw (RIP.Proxy @"vector_len") ptr0

instance Marshal.WriteRaw Vector_Aux where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_len2 ->
            HasCField.writeRaw (RIP.Proxy @"vector_len") ptr0 vector_len2

deriving via Marshal.EquivStorable Vector_Aux instance RIP.Storable Vector_Aux

instance HasCField.HasCField Vector_Aux "vector_len" where

  type CFieldType Vector_Aux "vector_len" = RIP.CInt

  offset# = \_ -> \_ -> 0

instance ( ((~) ty) RIP.CInt
         ) => RIP.HasField "vector_len" (RIP.Ptr Vector_Aux) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"vector_len")

instance FLAM.Offset RIP.CChar Vector_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct vector@

    __defined at:__ @manual\/zero_copy.h 72:8@

    __exported by:__ @manual\/zero_copy.h@
-}
type Vector = (FLAM.WithFlam RIP.CChar) Vector_Aux

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/zero_copy.h 82:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Triplet = Triplet
  { unwrapTriplet :: (CA.ConstantArray 3) RIP.CInt
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) RIP.CInt)
         ) => RIP.HasField "unwrapTriplet" (RIP.Ptr Triplet) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapTriplet")

instance HasCField.HasCField Triplet "unwrapTriplet" where

  type CFieldType Triplet "unwrapTriplet" =
    (CA.ConstantArray 3) RIP.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/zero_copy.h 83:17@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Matrix = Matrix
  { unwrapMatrix :: (CA.ConstantArray 3) Triplet
  }
  deriving stock (Eq, RIP.Generic, Show)
  deriving newtype
    ( Marshal.ReadRaw
    , Marshal.StaticSize
    , RIP.Storable
    , Marshal.WriteRaw
    )

instance ( ((~) ty) ((CA.ConstantArray 3) Triplet)
         ) => RIP.HasField "unwrapMatrix" (RIP.Ptr Matrix) (RIP.Ptr ty) where

  getField =
    HasCField.fromPtr (RIP.Proxy @"unwrapMatrix")

instance HasCField.HasCField Matrix "unwrapMatrix" where

  type CFieldType Matrix "unwrapMatrix" =
    (CA.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0
