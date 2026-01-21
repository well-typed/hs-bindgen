{-# LANGUAGE DataKinds #-}
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
import qualified GHC.Ptr as Ptr
import qualified GHC.Records
import qualified HsBindgen.Runtime.ByteArray
import qualified HsBindgen.Runtime.ConstantArray
import qualified HsBindgen.Runtime.FlexibleArrayMember
import qualified HsBindgen.Runtime.HasBaseForeignType
import qualified HsBindgen.Runtime.HasCField
import qualified HsBindgen.Runtime.SizedByteArray
import Data.Bits (FiniteBits)
import GHC.Exts ((*#), (+#))
import HsBindgen.Runtime.TypeEquality (TyEq)
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
  deriving stock (Eq, Show)

instance F.Storable Point where

  sizeOf = \_ -> (8 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Point
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"point_x") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"point_y") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"point_x") ptr0 point_x2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"point_y") ptr0 point_y3

instance Data.Primitive.Types.Prim Point where

  sizeOf# = \_ -> (8#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Point (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Point v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Point point_x4 point_y5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) point_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) point_y5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Point (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Point v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Point point_x4 point_y5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) point_x4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) point_y5 s6

instance HsBindgen.Runtime.HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "point_x")
         ) => GHC.Records.HasField "point_x" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"point_x")

instance HsBindgen.Runtime.HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = FC.CInt

  offset# = \_ -> \_ -> 4

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Point) "point_y")
         ) => GHC.Records.HasField "point_y" (Ptr.Ptr Point) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"point_y")

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
  deriving stock (Eq, Show)

instance F.Storable Rectangle where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Rectangle
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"rectangle_topleft") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"rectangle_bottomright") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Rectangle rectangle_topleft2 rectangle_bottomright3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"rectangle_topleft") ptr0 rectangle_topleft2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"rectangle_bottomright") ptr0 rectangle_bottomright3

instance Data.Primitive.Types.Prim Rectangle where

  sizeOf# = \_ -> (16#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Rectangle (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Rectangle v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Rectangle rectangle_topleft4 rectangle_bottomright5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) rectangle_topleft4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) rectangle_bottomright5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Rectangle (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Rectangle v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Rectangle rectangle_topleft4 rectangle_bottomright5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) rectangle_topleft4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) rectangle_bottomright5 s6

instance HsBindgen.Runtime.HasCField.HasCField Rectangle "rectangle_topleft" where

  type CFieldType Rectangle "rectangle_topleft" = Point

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rectangle) "rectangle_topleft")
         ) => GHC.Records.HasField "rectangle_topleft" (Ptr.Ptr Rectangle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"rectangle_topleft")

instance HsBindgen.Runtime.HasCField.HasCField Rectangle "rectangle_bottomright" where

  type CFieldType Rectangle "rectangle_bottomright" =
    Point

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Rectangle) "rectangle_bottomright")
         ) => GHC.Records.HasField "rectangle_bottomright" (Ptr.Ptr Rectangle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"rectangle_bottomright")

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
  deriving stock (Eq, Show)

instance F.Storable Circle where

  sizeOf = \_ -> (12 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Circle
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"circle_midpoint") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"circle_radius") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Circle circle_midpoint2 circle_radius3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"circle_midpoint") ptr0 circle_midpoint2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"circle_radius") ptr0 circle_radius3

instance Data.Primitive.Types.Prim Circle where

  sizeOf# = \_ -> (12#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Circle (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Circle v4 v6 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Circle circle_midpoint4 circle_radius5 ->
                case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (0#)) circle_midpoint4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (2#) i1) (1#)) circle_radius5 s6

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Circle (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) s3 of
                (# s5, v6 #) -> (# s5, Circle v4 v6 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Circle circle_midpoint4 circle_radius5 ->
                case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (0#)) circle_midpoint4 s3 of
                  s6 ->
                    Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (2#) i1) (1#)) circle_radius5 s6

instance HsBindgen.Runtime.HasCField.HasCField Circle "circle_midpoint" where

  type CFieldType Circle "circle_midpoint" = Point

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Circle) "circle_midpoint")
         ) => GHC.Records.HasField "circle_midpoint" (Ptr.Ptr Circle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"circle_midpoint")

instance HsBindgen.Runtime.HasCField.HasCField Circle "circle_radius" where

  type CFieldType Circle "circle_radius" = FC.CInt

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Circle) "circle_radius")
         ) => GHC.Records.HasField "circle_radius" (Ptr.Ptr Circle) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"circle_radius")

{-| __C declaration:__ @union shape@

    __defined at:__ @manual\/zero_copy.h 30:7@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Shape = Shape
  { un_Shape :: Data.Array.Byte.ByteArray
  }

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 16) 4 instance F.Storable Shape

deriving via (HsBindgen.Runtime.SizedByteArray.SizedByteArray 16) 4 instance Data.Primitive.Types.Prim Shape

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
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_shape_rectangle'

-}
set_shape_rectangle ::
     Rectangle
  -> Shape
set_shape_rectangle =
  HsBindgen.Runtime.ByteArray.setUnionPayload

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
  HsBindgen.Runtime.ByteArray.getUnionPayload

{-|

  __See:__ 'get_shape_circle'

-}
set_shape_circle ::
     Circle
  -> Shape
set_shape_circle =
  HsBindgen.Runtime.ByteArray.setUnionPayload

instance HsBindgen.Runtime.HasCField.HasCField Shape "shape_rectangle" where

  type CFieldType Shape "shape_rectangle" = Rectangle

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Shape) "shape_rectangle")
         ) => GHC.Records.HasField "shape_rectangle" (Ptr.Ptr Shape) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"shape_rectangle")

instance HsBindgen.Runtime.HasCField.HasCField Shape "shape_circle" where

  type CFieldType Shape "shape_circle" = Circle

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Shape) "shape_circle")
         ) => GHC.Records.HasField "shape_circle" (Ptr.Ptr Shape) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"shape_circle")

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
  deriving stock (Eq, Show)

instance F.Storable Colour where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Colour
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"colour_opacity") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"colour_brightness") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"colour_red") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"colour_green") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCBitfield (Data.Proxy.Proxy @"colour_blue") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Colour
            colour_opacity2
            colour_brightness3
            colour_red4
            colour_green5
            colour_blue6 ->
                 HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"colour_opacity") ptr0 colour_opacity2
              >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"colour_brightness") ptr0 colour_brightness3
              >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"colour_red") ptr0 colour_red4
              >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"colour_green") ptr0 colour_green5
              >> HsBindgen.Runtime.HasCField.pokeCBitfield (Data.Proxy.Proxy @"colour_blue") ptr0 colour_blue6

instance Data.Primitive.Types.Prim Colour where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Colour (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (5#) i1) (0#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (5#) i1) (1#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (5#) i1) (2#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (5#) i1) (3#))) (Data.Primitive.Types.indexByteArray# arr0 ((+#) ((*#) (5#) i1) (4#)))

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (5#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (5#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (5#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (5#) i1) (3#)) s7 of
                        (# s9, v10 #) ->
                          case Data.Primitive.Types.readByteArray# arr0 ((+#) ((*#) (5#) i1) (4#)) s9 of
                            (# s11, v12 #) -> (# s11, Colour v4 v6 v8 v10 v12 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Colour
                colour_opacity4
                colour_brightness5
                colour_red6
                colour_green7
                colour_blue8 ->
                  case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (5#) i1) (0#)) colour_opacity4 s3 of
                    s9 ->
                      case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (5#) i1) (1#)) colour_brightness5 s9 of
                        s10 ->
                          case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (5#) i1) (2#)) colour_red6 s10 of
                            s11 ->
                              case Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (5#) i1) (3#)) colour_green7 s11 of
                                s12 ->
                                  Data.Primitive.Types.writeByteArray# arr0 ((+#) ((*#) (5#) i1) (4#)) colour_blue8 s12

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Colour (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (5#) i1) (0#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (5#) i1) (1#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (5#) i1) (2#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (5#) i1) (3#))) (Data.Primitive.Types.indexOffAddr# addr0 ((+#) ((*#) (5#) i1) (4#)))

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (5#) i1) (0#)) s2 of
            (# s3, v4 #) ->
              case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (5#) i1) (1#)) s3 of
                (# s5, v6 #) ->
                  case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (5#) i1) (2#)) s5 of
                    (# s7, v8 #) ->
                      case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (5#) i1) (3#)) s7 of
                        (# s9, v10 #) ->
                          case Data.Primitive.Types.readOffAddr# addr0 ((+#) ((*#) (5#) i1) (4#)) s9 of
                            (# s11, v12 #) -> (# s11, Colour v4 v6 v8 v10 v12 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Colour
                colour_opacity4
                colour_brightness5
                colour_red6
                colour_green7
                colour_blue8 ->
                  case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (5#) i1) (0#)) colour_opacity4 s3 of
                    s9 ->
                      case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (5#) i1) (1#)) colour_brightness5 s9 of
                        s10 ->
                          case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (5#) i1) (2#)) colour_red6 s10 of
                            s11 ->
                              case Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (5#) i1) (3#)) colour_green7 s11 of
                                s12 ->
                                  Data.Primitive.Types.writeOffAddr# addr0 ((+#) ((*#) (5#) i1) (4#)) colour_blue8 s12

instance HsBindgen.Runtime.HasCField.HasCBitfield Colour "colour_opacity" where

  type CBitfieldType Colour "colour_opacity" = FC.CUInt

  bitOffset# = \_ -> \_ -> 0

  bitWidth# = \_ -> \_ -> 2

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType Colour) "colour_opacity")
         ) => GHC.Records.HasField "colour_opacity" (Ptr.Ptr Colour) ((HsBindgen.Runtime.HasCField.BitfieldPtr Colour) "colour_opacity") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"colour_opacity")

instance HsBindgen.Runtime.HasCField.HasCBitfield Colour "colour_brightness" where

  type CBitfieldType Colour "colour_brightness" =
    FC.CUInt

  bitOffset# = \_ -> \_ -> 2

  bitWidth# = \_ -> \_ -> 3

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType Colour) "colour_brightness")
         ) => GHC.Records.HasField "colour_brightness" (Ptr.Ptr Colour) ((HsBindgen.Runtime.HasCField.BitfieldPtr Colour) "colour_brightness") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"colour_brightness")

instance HsBindgen.Runtime.HasCField.HasCBitfield Colour "colour_red" where

  type CBitfieldType Colour "colour_red" = FC.CUInt

  bitOffset# = \_ -> \_ -> 5

  bitWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType Colour) "colour_red")
         ) => GHC.Records.HasField "colour_red" (Ptr.Ptr Colour) ((HsBindgen.Runtime.HasCField.BitfieldPtr Colour) "colour_red") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"colour_red")

instance HsBindgen.Runtime.HasCField.HasCBitfield Colour "colour_green" where

  type CBitfieldType Colour "colour_green" = FC.CUInt

  bitOffset# = \_ -> \_ -> 13

  bitWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType Colour) "colour_green")
         ) => GHC.Records.HasField "colour_green" (Ptr.Ptr Colour) ((HsBindgen.Runtime.HasCField.BitfieldPtr Colour) "colour_green") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"colour_green")

instance HsBindgen.Runtime.HasCField.HasCBitfield Colour "colour_blue" where

  type CBitfieldType Colour "colour_blue" = FC.CUInt

  bitOffset# = \_ -> \_ -> 21

  bitWidth# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CBitfieldType Colour) "colour_blue")
         ) => GHC.Records.HasField "colour_blue" (Ptr.Ptr Colour) ((HsBindgen.Runtime.HasCField.BitfieldPtr Colour) "colour_blue") where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCBitfield (Data.Proxy.Proxy @"colour_blue")

{-| __C declaration:__ @myInt@

    __defined at:__ @manual\/zero_copy.h 50:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype MyInt = MyInt
  { un_MyInt :: FC.CInt
  }
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (F.Storable, HsBindgen.Runtime.HasBaseForeignType.HasBaseForeignType, Data.Primitive.Types.Prim, Bits.Bits, Bounded, Enum, FiniteBits, Integral, Ix.Ix, Num, Real)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType MyInt) "un_MyInt")
         ) => GHC.Records.HasField "un_MyInt" (Ptr.Ptr MyInt) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_MyInt")

instance HsBindgen.Runtime.HasCField.HasCField MyInt "un_MyInt" where

  type CFieldType MyInt "un_MyInt" = FC.CInt

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
  deriving stock (Eq, Show)

instance F.Storable Drawing where

  sizeOf = \_ -> (16 :: Int)

  alignment = \_ -> (8 :: Int)

  peek =
    \ptr0 ->
          pure Drawing
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"drawing_shape") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"drawing_colour") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Drawing drawing_shape2 drawing_colour3 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"drawing_shape") ptr0 drawing_shape2
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"drawing_colour") ptr0 drawing_colour3

instance HsBindgen.Runtime.HasCField.HasCField Drawing "drawing_shape" where

  type CFieldType Drawing "drawing_shape" =
    Ptr.Ptr Shape

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Drawing) "drawing_shape")
         ) => GHC.Records.HasField "drawing_shape" (Ptr.Ptr Drawing) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"drawing_shape")

instance HsBindgen.Runtime.HasCField.HasCField Drawing "drawing_colour" where

  type CFieldType Drawing "drawing_colour" =
    Ptr.Ptr Colour

  offset# = \_ -> \_ -> 8

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Drawing) "drawing_colour")
         ) => GHC.Records.HasField "drawing_colour" (Ptr.Ptr Drawing) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"drawing_colour")

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
  deriving stock (Eq, Show)

instance F.Storable Tic_tac_toe where

  sizeOf = \_ -> (36 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Tic_tac_toe
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"tic_tac_toe_row1") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"tic_tac_toe_row2") ptr0
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"tic_tac_toe_row3") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Tic_tac_toe tic_tac_toe_row12 tic_tac_toe_row23 tic_tac_toe_row34 ->
               HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"tic_tac_toe_row1") ptr0 tic_tac_toe_row12
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"tic_tac_toe_row2") ptr0 tic_tac_toe_row23
            >> HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"tic_tac_toe_row3") ptr0 tic_tac_toe_row34

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row1" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row1" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row1")
         ) => GHC.Records.HasField "tic_tac_toe_row1" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"tic_tac_toe_row1")

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row2" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row2" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 12

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row2")
         ) => GHC.Records.HasField "tic_tac_toe_row2" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"tic_tac_toe_row2")

instance HsBindgen.Runtime.HasCField.HasCField Tic_tac_toe "tic_tac_toe_row3" where

  type CFieldType Tic_tac_toe "tic_tac_toe_row3" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 24

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Tic_tac_toe) "tic_tac_toe_row3")
         ) => GHC.Records.HasField "tic_tac_toe_row3" (Ptr.Ptr Tic_tac_toe) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"tic_tac_toe_row3")

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
  deriving stock (Eq, Show)

instance F.Storable Vector_Aux where

  sizeOf = \_ -> (4 :: Int)

  alignment = \_ -> (4 :: Int)

  peek =
    \ptr0 ->
          pure Vector
      <*> HsBindgen.Runtime.HasCField.peekCField (Data.Proxy.Proxy @"vector_len") ptr0

  poke =
    \ptr0 ->
      \s1 ->
        case s1 of
          Vector vector_len2 ->
            HsBindgen.Runtime.HasCField.pokeCField (Data.Proxy.Proxy @"vector_len") ptr0 vector_len2

instance Data.Primitive.Types.Prim Vector_Aux where

  sizeOf# = \_ -> (4#)

  alignment# = \_ -> (4#)

  indexByteArray# =
    \arr0 ->
      \i1 ->
        Vector (Data.Primitive.Types.indexByteArray# arr0 i1)

  readByteArray# =
    \arr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readByteArray# arr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Vector v4 #)

  writeByteArray# =
    \arr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Vector vector_len4 ->
                Data.Primitive.Types.writeByteArray# arr0 i1 vector_len4 s3

  indexOffAddr# =
    \addr0 ->
      \i1 ->
        Vector (Data.Primitive.Types.indexOffAddr# addr0 i1)

  readOffAddr# =
    \addr0 ->
      \i1 ->
        \s2 ->
          case Data.Primitive.Types.readOffAddr# addr0 i1 s2 of
            (# s3, v4 #) -> (# s3, Vector v4 #)

  writeOffAddr# =
    \addr0 ->
      \i1 ->
        \struct2 ->
          \s3 ->
            case struct2 of
              Vector vector_len4 ->
                Data.Primitive.Types.writeOffAddr# addr0 i1 vector_len4 s3

instance HsBindgen.Runtime.HasCField.HasCField Vector_Aux "vector_len" where

  type CFieldType Vector_Aux "vector_len" = FC.CInt

  offset# = \_ -> \_ -> 0

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Vector_Aux) "vector_len")
         ) => GHC.Records.HasField "vector_len" (Ptr.Ptr Vector_Aux) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"vector_len")

instance HsBindgen.Runtime.FlexibleArrayMember.Offset FC.CChar Vector_Aux where

  offset = \_ty0 -> 4

{-| __C declaration:__ @struct vector@

    __defined at:__ @manual\/zero_copy.h 72:8@

    __exported by:__ @manual\/zero_copy.h@
-}
type Vector =
  (HsBindgen.Runtime.FlexibleArrayMember.WithFlexibleArrayMember FC.CChar) Vector_Aux

{-| __C declaration:__ @triplet@

    __defined at:__ @manual\/zero_copy.h 82:13@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Triplet = Triplet
  { un_Triplet :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Triplet) "un_Triplet")
         ) => GHC.Records.HasField "un_Triplet" (Ptr.Ptr Triplet) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Triplet")

instance HsBindgen.Runtime.HasCField.HasCField Triplet "un_Triplet" where

  type CFieldType Triplet "un_Triplet" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) FC.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @matrix@

    __defined at:__ @manual\/zero_copy.h 83:17@

    __exported by:__ @manual\/zero_copy.h@
-}
newtype Matrix = Matrix
  { un_Matrix :: (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet
  }
  deriving stock (Eq, Show)
  deriving newtype (F.Storable)

instance ( TyEq ty ((HsBindgen.Runtime.HasCField.CFieldType Matrix) "un_Matrix")
         ) => GHC.Records.HasField "un_Matrix" (Ptr.Ptr Matrix) (Ptr.Ptr ty) where

  getField =
    HsBindgen.Runtime.HasCField.ptrToCField (Data.Proxy.Proxy @"un_Matrix")

instance HsBindgen.Runtime.HasCField.HasCField Matrix "un_Matrix" where

  type CFieldType Matrix "un_Matrix" =
    (HsBindgen.Runtime.ConstantArray.ConstantArray 3) Triplet

  offset# = \_ -> \_ -> 0
