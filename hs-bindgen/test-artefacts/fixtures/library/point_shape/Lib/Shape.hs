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

module Lib.Shape
    ( Lib.Shape.Shape(..)
    , Lib.Shape.On_draw_Aux(..)
    , Lib.Shape.On_draw(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified Lib.Point
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure, type (~))
import {-# SOURCE #-} Lib.Shape.Callback

{-| __C declaration:__ @struct shape@

    __defined at:__ @shape.h 5:8@

    __exported by:__ @shape.h@
-}
data Shape = Shape
  { shape_centre :: Lib.Point.Point
    {- ^ __C declaration:__ @centre@

         __defined at:__ @shape.h 5:29@

         __exported by:__ @shape.h@
    -}
  , shape_sides :: BG.CInt
    {- ^ __C declaration:__ @sides@

         __defined at:__ @shape.h 5:41@

         __exported by:__ @shape.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Shape where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Shape where

  readRaw =
    \ptr0 ->
          pure Shape
      <*> HasCField.readRaw (BG.Proxy @"shape_centre") ptr0
      <*> HasCField.readRaw (BG.Proxy @"shape_sides") ptr0

instance Marshal.WriteRaw Shape where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Shape shape_centre2 shape_sides3 ->
               HasCField.writeRaw (BG.Proxy @"shape_centre") ptr0 shape_centre2
            >> HasCField.writeRaw (BG.Proxy @"shape_sides") ptr0 shape_sides3

deriving via Marshal.EquivStorable Shape instance BG.Storable Shape

deriving via Struct.IsStructViaReadRaw Shape instance Struct.IsStruct Shape

{-| __C declaration:__ @centre@

    __defined at:__ @shape.h 5:29@

    __exported by:__ @shape.h@
-}
instance ( ty ~ Lib.Point.Point
         ) => BG.CompatHasField.HasField "shape_centre" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_centre = y1, shape_sides = BG.getField @"shape_sides" x0}
      , BG.getField @"shape_centre" x0
      )

instance ( ty ~ Lib.Point.Point
         ) => BG.HasField "shape_centre" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_centre")

instance HasCField.HasCField Shape "shape_centre" where

  type CFieldType Shape "shape_centre" =
    Lib.Point.Point

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @sides@

    __defined at:__ @shape.h 5:41@

    __exported by:__ @shape.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "shape_sides" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_sides = y1, shape_centre = BG.getField @"shape_centre" x0}
      , BG.getField @"shape_sides" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "shape_sides" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_sides")

instance HasCField.HasCField Shape "shape_sides" where

  type CFieldType Shape "shape_sides" = BG.CInt

  offset# = \_ -> \_ -> 16

{-| Auxiliary type used by 'On_draw'

    __C declaration:__ @on_draw@

    __defined at:__ @shape.h 6:18@

    __exported by:__ @shape.h@
-}
newtype On_draw_Aux = On_draw_Aux
  { unwrapOn_draw_Aux :: BG.Ptr Shape -> IO Lib.Point.Status
  }
  deriving stock (BG.Generic)

instance BG.ToFunPtr On_draw_Aux where

  toFunPtr = hs_bindgen_e0535817c449f9e4

instance BG.FromFunPtr On_draw_Aux where

  fromFunPtr = hs_bindgen_7a6697890840e122

instance ( ty ~ (BG.Ptr Shape -> IO Lib.Point.Status)
         ) => BG.CompatHasField.HasField "unwrapOn_draw_Aux" On_draw_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> On_draw_Aux {unwrapOn_draw_Aux = y1}
      , BG.getField @"unwrapOn_draw_Aux" x0
      )

instance ( ty ~ (BG.Ptr Shape -> IO Lib.Point.Status)
         ) => BG.HasField "unwrapOn_draw_Aux" (BG.Ptr On_draw_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_draw_Aux")

instance HasCField.HasCField On_draw_Aux "unwrapOn_draw_Aux" where

  type CFieldType On_draw_Aux "unwrapOn_draw_Aux" =
    BG.Ptr Shape -> IO Lib.Point.Status

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @on_draw@

    __defined at:__ @shape.h 6:18@

    __exported by:__ @shape.h@
-}
newtype On_draw = On_draw
  { unwrapOn_draw :: BG.FunPtr On_draw_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr On_draw_Aux
         ) => BG.CompatHasField.HasField "unwrapOn_draw" On_draw ty where

  hasField =
    \x0 ->
      (\y1 ->
         On_draw {unwrapOn_draw = y1}, BG.getField @"unwrapOn_draw" x0)

instance ( ty ~ BG.FunPtr On_draw_Aux
         ) => BG.HasField "unwrapOn_draw" (BG.Ptr On_draw) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapOn_draw")

instance HasCField.HasCField On_draw "unwrapOn_draw" where

  type CFieldType On_draw "unwrapOn_draw" =
    BG.FunPtr On_draw_Aux

  offset# = \_ -> \_ -> 0
