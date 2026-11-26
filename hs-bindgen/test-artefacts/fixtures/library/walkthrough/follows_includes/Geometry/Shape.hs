{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Geometry.Shape
    ( Geometry.Shape.sHAPE_MAX_POINTS
    , Geometry.Shape.Shape(..)
    , Geometry.Shape.Shape_filter_Aux(..)
    , Geometry.Shape.Shape_filter(..)
    )
  where

import qualified Geometry.Point
import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.PtrConst as PtrConst
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import Prelude ((<*>), (>>), Eq, IO, Int, Ord, Show, fmap, pure, type (~))

{-| __C declaration:__ @macro SHAPE_MAX_POINTS@

    __defined at:__ @..\/shape.h 6:9@

    __exported by:__ @follows_includes.h@
-}
sHAPE_MAX_POINTS :: BG.CInt
sHAPE_MAX_POINTS = (64 :: BG.CInt)

{-| __C declaration:__ @struct shape@

    __defined at:__ @..\/shape.h 8:8@

    __exported by:__ @follows_includes.h@
-}
data Shape = Shape
  { shape_points :: BG.Ptr Geometry.Point.Point
    {- ^ __C declaration:__ @points@

         __defined at:__ @..\/shape.h 9:17@

         __exported by:__ @follows_includes.h@
    -}
  , shape_count :: BG.CInt
    {- ^ __C declaration:__ @count@

         __defined at:__ @..\/shape.h 10:7@

         __exported by:__ @follows_includes.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Shape where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Shape where

  readRaw =
    \ptr0 ->
          pure Shape
      <*> HasCField.readRaw (BG.Proxy @"shape_points") ptr0
      <*> HasCField.readRaw (BG.Proxy @"shape_count") ptr0

instance Marshal.WriteRaw Shape where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Shape shape_points2 shape_count3 ->
               HasCField.writeRaw (BG.Proxy @"shape_points") ptr0 shape_points2
            >> HasCField.writeRaw (BG.Proxy @"shape_count") ptr0 shape_count3

deriving via Marshal.EquivStorable Shape instance BG.Storable Shape

deriving via Struct.IsStructViaReadRaw Shape instance Struct.IsStruct Shape

{-| __C declaration:__ @points@

    __defined at:__ @..\/shape.h 9:17@

    __exported by:__ @follows_includes.h@
-}
instance ( ty ~ BG.Ptr Geometry.Point.Point
         ) => BG.CompatHasField.HasField "shape_points" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_points = y1, shape_count = BG.getField @"shape_count" x0}
      , BG.getField @"shape_points" x0
      )

instance ( ty ~ BG.Ptr Geometry.Point.Point
         ) => BG.HasField "shape_points" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_points")

instance HasCField.HasCField Shape "shape_points" where

  type CFieldType Shape "shape_points" =
    BG.Ptr Geometry.Point.Point

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @count@

    __defined at:__ @..\/shape.h 10:7@

    __exported by:__ @follows_includes.h@
-}
instance ( ty ~ BG.CInt
         ) => BG.CompatHasField.HasField "shape_count" Shape ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape {shape_count = y1, shape_points = BG.getField @"shape_points" x0}
      , BG.getField @"shape_count" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "shape_count" (BG.Ptr Shape) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"shape_count")

instance HasCField.HasCField Shape "shape_count" where

  type CFieldType Shape "shape_count" = BG.CInt

  offset# = \_ -> \_ -> 8

{-| Auxiliary type used by 'Shape_filter'

    __C declaration:__ @shape_filter@

    __defined at:__ @..\/shape.h 14:15@

    __exported by:__ @follows_includes.h@
-}
newtype Shape_filter_Aux = Shape_filter_Aux
  { unwrapShape_filter_Aux :: PtrConst.PtrConst Shape -> BG.Ptr BG.Void -> IO BG.CInt
  }
  deriving stock (BG.Generic)

instance ( ty ~ (PtrConst.PtrConst Shape -> BG.Ptr BG.Void -> IO BG.CInt)
         ) => BG.CompatHasField.HasField "unwrapShape_filter_Aux" Shape_filter_Aux ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Shape_filter_Aux {unwrapShape_filter_Aux = y1}
      , BG.getField @"unwrapShape_filter_Aux" x0
      )

instance ( ty ~ (PtrConst.PtrConst Shape -> BG.Ptr BG.Void -> IO BG.CInt)
         ) => BG.HasField "unwrapShape_filter_Aux" (BG.Ptr Shape_filter_Aux) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShape_filter_Aux")

instance HasCField.HasCField Shape_filter_Aux "unwrapShape_filter_Aux" where

  type CFieldType Shape_filter_Aux "unwrapShape_filter_Aux" =
    PtrConst.PtrConst Shape -> BG.Ptr BG.Void -> IO BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @shape_filter@

    __defined at:__ @..\/shape.h 14:15@

    __exported by:__ @follows_includes.h@
-}
newtype Shape_filter = Shape_filter
  { unwrapShape_filter :: BG.FunPtr Shape_filter_Aux
  }
  deriving stock (Eq, BG.Generic, Ord, Show)
  deriving newtype
    ( BG.HasFFIType
    , Marshal.ReadRaw
    , Marshal.StaticSize
    , BG.Storable
    , Marshal.WriteRaw
    )

instance ( ty ~ BG.FunPtr Shape_filter_Aux
         ) => BG.CompatHasField.HasField "unwrapShape_filter" Shape_filter ty where

  hasField =
    \x0 ->
      ( \y1 -> Shape_filter {unwrapShape_filter = y1}
      , BG.getField @"unwrapShape_filter" x0
      )

instance ( ty ~ BG.FunPtr Shape_filter_Aux
         ) => BG.HasField "unwrapShape_filter" (BG.Ptr Shape_filter) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"unwrapShape_filter")

instance HasCField.HasCField Shape_filter "unwrapShape_filter" where

  type CFieldType Shape_filter "unwrapShape_filter" =
    BG.FunPtr Shape_filter_Aux

  offset# = \_ -> \_ -> 0

-- __unique:__ @toShape_filter_Aux@
foreign import ccall safe "wrapper" hs_bindgen_4eccd3cc983643a6_base ::
     (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO BG.CInt)
  -> IO (BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO BG.CInt))

-- __unique:__ @toShape_filter_Aux@
hs_bindgen_4eccd3cc983643a6 ::
     Shape_filter_Aux
  -> IO (BG.FunPtr Shape_filter_Aux)
hs_bindgen_4eccd3cc983643a6 =
  \fun0 ->
    fmap BG.castFunPtr (hs_bindgen_4eccd3cc983643a6_base (\x1 ->
                                                            \x2 ->
                                                              fmap BG.toFFIType (BG.getField @"unwrapShape_filter_Aux" fun0 (BG.fromFFIType x1) (BG.fromFFIType x2))))

-- __unique:__ @fromShape_filter_Aux@
foreign import ccall safe "dynamic" hs_bindgen_13fbaa03404fc86a_base ::
     BG.FunPtr (BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO BG.CInt)
  -> BG.Ptr BG.Void -> BG.Ptr BG.Void -> IO BG.CInt

-- __unique:__ @fromShape_filter_Aux@
hs_bindgen_13fbaa03404fc86a ::
     BG.FunPtr Shape_filter_Aux
  -> Shape_filter_Aux
hs_bindgen_13fbaa03404fc86a =
  \funPtr0 ->
    Shape_filter_Aux (\x1 ->
                        \x2 ->
                          fmap BG.fromFFIType (hs_bindgen_13fbaa03404fc86a_base (BG.castFunPtr funPtr0) (BG.toFFIType x1) (BG.toFFIType x2)))

instance BG.ToFunPtr Shape_filter_Aux where

  toFunPtr = hs_bindgen_4eccd3cc983643a6

instance BG.FromFunPtr Shape_filter_Aux where

  fromFunPtr = hs_bindgen_13fbaa03404fc86a
