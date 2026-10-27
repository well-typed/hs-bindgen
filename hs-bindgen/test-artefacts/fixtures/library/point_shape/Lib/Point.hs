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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Point
    ( Lib.Point.Status(..)
    , Lib.Point.Point(..)
    )
  where

import qualified HsBindgen.Runtime.HasCField as HasCField
import qualified HsBindgen.Runtime.Marshal as Marshal
import qualified HsBindgen.Runtime.Struct as Struct
import qualified HsBindgen.Runtime.Support as BG
import qualified HsBindgen.Runtime.Support.CompatHasField as BG.CompatHasField
import {-# SOURCE #-} qualified Lib.Shape
import Prelude ((<*>), (>>), Bounded, Enum, Eq, Int, Integral, Num, Ord, Read, Real, Show, pure, type (~))

{-| __C declaration:__ @status@

    __defined at:__ @point.h 4:13@

    __exported by:__ @shape.h@
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

{-| __C declaration:__ @struct point@

    __defined at:__ @point.h 6:8@

    __exported by:__ @shape.h@
-}
data Point = Point
  { point_x :: BG.CInt
    {- ^ __C declaration:__ @x@

         __defined at:__ @point.h 6:20@

         __exported by:__ @shape.h@
    -}
  , point_y :: BG.CInt
    {- ^ __C declaration:__ @y@

         __defined at:__ @point.h 6:27@

         __exported by:__ @shape.h@
    -}
  , point_owner :: BG.Ptr Lib.Shape.Shape
    {- ^ __C declaration:__ @owner@

         __defined at:__ @point.h 6:44@

         __exported by:__ @shape.h@
    -}
  }
  deriving stock (Eq, BG.Generic, Show)

instance Marshal.StaticSize Point where

  staticSizeOf = \_ -> (16 :: Int)

  staticAlignment = \_ -> (8 :: Int)

instance Marshal.ReadRaw Point where

  readRaw =
    \ptr0 ->
          pure Point
      <*> HasCField.readRaw (BG.Proxy @"point_x") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point_y") ptr0
      <*> HasCField.readRaw (BG.Proxy @"point_owner") ptr0

instance Marshal.WriteRaw Point where

  writeRaw =
    \ptr0 ->
      \s1 ->
        case s1 of
          Point point_x2 point_y3 point_owner4 ->
               HasCField.writeRaw (BG.Proxy @"point_x") ptr0 point_x2
            >> HasCField.writeRaw (BG.Proxy @"point_y") ptr0 point_y3
            >> HasCField.writeRaw (BG.Proxy @"point_owner") ptr0 point_owner4

deriving via Marshal.EquivStorable Point instance BG.Storable Point

deriving via Struct.IsStructViaReadRaw Point instance Struct.IsStruct Point

{-| __C declaration:__ @x@

    __defined at:__ @point.h 6:20@

    __exported by:__ @shape.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_x" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point { point_x = y1
                , point_y = BG.getField @"point_y" x0
                , point_owner = BG.getField @"point_owner" x0
                }
      , BG.getField @"point_x" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_x" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_x")

instance HasCField.HasCField Point "point_x" where

  type CFieldType Point "point_x" = BG.CInt

  offset# = \_ -> \_ -> 0

{-| __C declaration:__ @y@

    __defined at:__ @point.h 6:27@

    __exported by:__ @shape.h@
-}
instance (ty ~ BG.CInt) => BG.CompatHasField.HasField "point_y" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point { point_y = y1
                , point_x = BG.getField @"point_x" x0
                , point_owner = BG.getField @"point_owner" x0
                }
      , BG.getField @"point_y" x0
      )

instance ( ty ~ BG.CInt
         ) => BG.HasField "point_y" (BG.Ptr Point) (BG.Ptr ty) where

  getField = HasCField.fromPtr (BG.Proxy @"point_y")

instance HasCField.HasCField Point "point_y" where

  type CFieldType Point "point_y" = BG.CInt

  offset# = \_ -> \_ -> 4

{-| __C declaration:__ @owner@

    __defined at:__ @point.h 6:44@

    __exported by:__ @shape.h@
-}
instance ( ty ~ BG.Ptr Lib.Shape.Shape
         ) => BG.CompatHasField.HasField "point_owner" Point ty where

  hasField =
    \x0 ->
      ( \y1 ->
          Point { point_owner = y1
                , point_x = BG.getField @"point_x" x0
                , point_y = BG.getField @"point_y" x0
                }
      , BG.getField @"point_owner" x0
      )

instance ( ty ~ BG.Ptr Lib.Shape.Shape
         ) => BG.HasField "point_owner" (BG.Ptr Point) (BG.Ptr ty) where

  getField =
    HasCField.fromPtr (BG.Proxy @"point_owner")

instance HasCField.HasCField Point "point_owner" where

  type CFieldType Point "point_owner" =
    BG.Ptr Lib.Shape.Shape

  offset# = \_ -> \_ -> 8
