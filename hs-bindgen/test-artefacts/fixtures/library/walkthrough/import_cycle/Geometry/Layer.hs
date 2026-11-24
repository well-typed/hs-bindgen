{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
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

module Geometry.Layer
    ( Geometry.Layer.lAYER_MAX_SHAPES
    , Geometry.Layer.Layer(..)
    , Geometry.Layer.Layer_paint_Aux(..)
    , Geometry.Layer.Layer_paint(..)
    )
  where

import qualified Geometry.Shape
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified Geometry.Scene
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, pure, type (~))

{-| __C declaration:__ @macro LAYER_MAX_SHAPES@

    __defined at:__ @geometry\/layer.h 10:9@

    __exported by:__ @import_cycle.h@
-}
lAYER_MAX_SHAPES :: BG.CInt
lAYER_MAX_SHAPES = (32 :: BG.CInt)

{-| __C declaration:__ @struct layer@

    __defined at:__ @geometry\/layer.h 14:8@

    __exported by:__ @import_cycle.h@
-}
data Layer = Layer
  { layer_scene :: BG.Ptr Geometry.Scene.Scene
    {- ^ __C declaration:__ @scene@

         __defined at:__ @geometry\/layer.h 15:17@

         __exported by:__ @import_cycle.h@
    -}
  , layer_shapes :: BG.Ptr Geometry.Shape.Shape
    {- ^ __C declaration:__ @shapes@

         __defined at:__ @geometry\/layer.h 16:17@

         __exported by:__ @import_cycle.h@
    -}
  , layer_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @geometry\/layer.h 17:7@

         __exported by:__ @import_cycle.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Layer where

  staticSizeOf = \_ -> (24 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Layer where

  readRaw =
    \ptr0 ->
          pure Layer
      <*> HasCField.readRaw (BG.Proxy @"layer_scene") ptr0
      <*> HasCField.readRaw (BG.Proxy @"layer_shapes") ptr0
      <*> HasCField.readRaw (BG.Proxy @"layer_count") ptr0

instance Marshal.WriteRaw Layer where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Layer layer_scene2 layer_shapes3 layer_count4 ->
               HasCField.writeRaw (BG.Proxy @"layer_scene") ptr0 layer_scene2
            >> HasCField.writeRaw (BG.Proxy @"layer_shapes") ptr0 layer_shapes3
            >> HasCField.writeRaw (BG.Proxy @"layer_count") ptr0 layer_count4

deriving via Marshal.EquivStorable Layer instance BG.Storable Layer

deriving via Struct.IsStructViaReadRaw Layer instance Struct.IsStruct Layer

{-| __C declaration:__ @scene@

    __defined at:__ @geometry\/layer.h 15:17@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.Ptr Geometry.Scene.Scene
         ) => BG.CompatHasField.HasField "layer_scene" Layer ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Layer { layer_scene = y1
                , layer_shapes = BG.getField @"layer_shapes" x0
                , layer_count = BG.getField @"layer_count" x0
                }
      , BG.getField @"layer_scene" x0
      )

instance ( ty ~ BG.Ptr Geometry.Scene.Scene
         ) => BG.HasField "layer_scene" (BG.Ptr Layer) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"layer_scene")

instance HasCField.HasCField Layer "layer_scene" where

  type CFieldType Layer "layer_scene" =
    BG.Ptr Geometry.Scene.Scene

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @shapes@

    __defined at:__ @geometry\/layer.h 16:17@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.Ptr Geometry.Shape.Shape
         ) => BG.CompatHasField.HasField "layer_shapes" Layer ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Layer { layer_shapes = y1
                , layer_scene = BG.getField @"layer_scene" x0
                , layer_count = BG.getField @"layer_count" x0
                }
      , BG.getField @"layer_shapes" x0
      )

instance ( ty ~ BG.Ptr Geometry.Shape.Shape
         ) => BG.HasField "layer_shapes" (BG.Ptr Layer) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"layer_shapes")

instance HasCField.HasCField Layer "layer_shapes" where

  type CFieldType Layer "layer_shapes" =
    BG.Ptr Geometry.Shape.Shape

  offset# = \_ -> \_ -> 8

{-| __C declaration:__ @count@

    __defined at:__ @geometry\/layer.h 17:7@

    __exported by:__ @import_cycle.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "layer_count" Layer ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Layer { layer_count = y1
                , layer_scene = BG.getField @"layer_scene" x0
                , layer_shapes = BG.getField @"layer_shapes" x0
                }
      , BG.getField @"layer_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "layer_count" (BG.Ptr Layer) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"layer_count")

instance HasCField.HasCField Layer "layer_count" where

  type CFieldType Layer "layer_count" = BG.CInt

  offset# = \_ -> \_ -> 16

{-| Auxiliary type used by 'Layer_paint'

    __C declaration:__ @layer_paint@

    __defined at:__ @geometry\/layer.h 22:16@

    __exported by:__ @import_cycle.h@
-}
newtype Layer_paint_Aux = Layer_paint_Aux
  { unwrapLayer_paint_Aux :: BG.Ptr Layer -> BG.Ptr BG.Void -> IO ()
  }
  deriving stock (BG.Generic)

instance ( ty ~ (BG.Ptr Layer -> BG.Ptr BG.Void -> IO ())
         ) => BG.CompatHasField.HasField "unwrapLayer_paint_Aux" Layer_paint_Aux ty where

  hasField =
    \x0 ->
      ( \y1 -> Layer_paint_Aux {unwrapLayer_paint_Aux = y1}
      , BG.getField @"unwrapLayer_paint_Aux" x0
      )

instance ( ty ~ (BG.Ptr Layer -> BG.Ptr BG.Void -> IO ())
         ) => BG.HasField "unwrapLayer_paint_Aux" (BG.Ptr Layer_paint_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapLayer_paint_Aux")

instance HasCField.HasCField Layer_paint_Aux "unwrapLayer_paint_Aux" where

  type CFieldType Layer_paint_Aux "unwrapLayer_paint_Aux" =
    BG.Ptr Layer -> BG.Ptr BG.Void -> IO ()

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @layer_paint@

    __defined at:__ @geometry\/layer.h 22:16@

    __exported by:__ @import_cycle.h@
-}
newtype Layer_paint = Layer_paint
  { unwrapLayer_paint :: BG.FunPtr Layer_paint_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Layer_paint_Aux
         ) => BG.CompatHasField.HasField "unwrapLayer_paint" Layer_paint ty where

  hasField =
    \x0 ->
      ( \y1 -> Layer_paint {unwrapLayer_paint = y1}
      , BG.getField @"unwrapLayer_paint" x0
      )

instance ( ty ~ BG.FunPtr Layer_paint_Aux
         ) => BG.HasField "unwrapLayer_paint" (BG.Ptr Layer_paint) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapLayer_paint")

instance HasCField.HasCField Layer_paint "unwrapLayer_paint" where

  type CFieldType Layer_paint "unwrapLayer_paint" =
    BG.FunPtr Layer_paint_Aux

  offset# = \_ -> \_ -> 0
